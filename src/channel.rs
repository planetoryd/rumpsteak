use futures::{channel::mpsc, Future, Sink, SinkExt, Stream};
use futures_util::StreamExt;
use thiserror::Error;
use std::{
    pin::Pin,
    task::{Context, Poll},
};

pub trait Pair<P: Pair<Self>>: Sized {
    fn pair() -> (Self, P);
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Nil;

impl Pair<Nil> for Nil {
    #[inline]
    fn pair() -> (Nil, Nil) {
        (Nil, Nil)
    }
}

impl<T> Pair<mpsc::UnboundedReceiver<T>> for mpsc::UnboundedSender<T> {
    fn pair() -> (Self, mpsc::UnboundedReceiver<T>) {
        mpsc::unbounded()
    }
}

impl<T> Pair<mpsc::UnboundedSender<T>> for mpsc::UnboundedReceiver<T> {
    fn pair() -> (Self, mpsc::UnboundedSender<T>) {
        let (sender, receiver) = Pair::pair();
        (receiver, sender)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bidirectional<S, R> {
    sender: S,
    receiver: R,
}

impl<S, R> Bidirectional<S, R> {
    pub fn new(sender: S, receiver: R) -> Self {
        Self { sender, receiver }
    }
}

impl<S: Pair<R>, R: Pair<S>> Pair<Self> for Bidirectional<S, R> {
    fn pair() -> (Self, Self) {
        let (left_sender, right_receiver) = Pair::pair();
        let (right_sender, left_receiver) = Pair::pair();
        (
            Bidirectional::new(left_sender, left_receiver),
            Bidirectional::new(right_sender, right_receiver),
        )
    }
}

impl<S: Unpin, R: Unpin> Bidirectional<S, R> {
    fn sender(self: Pin<&mut Self>) -> Pin<&mut S> {
        Pin::new(&mut self.get_mut().sender)
    }

    fn receiver(self: Pin<&mut Self>) -> Pin<&mut R> {
        Pin::new(&mut self.get_mut().receiver)
    }
}

impl<T, S: Sink<T> + Unpin, R: Unpin> Sink<T> for Bidirectional<S, R> {
    type Error = S::Error;

    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        S::poll_ready(self.sender(), cx)
    }

    fn start_send(self: Pin<&mut Self>, item: T) -> Result<(), Self::Error> {
        S::start_send(self.sender(), item)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        S::poll_flush(self.sender(), cx)
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        S::poll_close(self.sender(), cx)
    }
}

impl<S: Unpin, R: Stream + Unpin> Stream for Bidirectional<S, R> {
    type Item = R::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        R::poll_next(self.receiver(), cx)
    }
}

pub trait Sending<Item> {
    type Fut<'x>: Future<Output = Result<(), Self::Error>>
    where
        Self: 'x;
    type Error;
    fn send(&mut self, item: Item) -> Self::Fut<'_>;
}

pub trait Recving<Item> {
    type Fut<'x>: Future<Output = Result<Option<Item>, Self::Error>>
    where
        Self: 'x;
    type Error;
    fn recv(&mut self) -> Self::Fut<'_>;
}

impl<I, T: Sink<I> + Unpin> Sending<I> for T {
    type Fut<'x> = futures_util::sink::Send<'x, Self, I> where T: 'x;
    type Error = <Self as Sink<I>>::Error;
    fn send(&mut self, item: I) -> Self::Fut<'_> {
        SinkExt::send(self, item)
    }
}
impl<I, T: Stream<Item = I> + Unpin> Recving<I> for T {
    type Fut<'x> = impl Future<Output = Result<Option<I>, Self::Error>> where T: 'x;
    type Error = EmptyErr;
    fn recv(&mut self) -> Self::Fut<'_> {
        async move { Result::<_, _>::Ok(StreamExt::next(self).await) }
    }
}

#[derive(Error, Debug)]
#[error("never happens")]
pub struct EmptyErr;

