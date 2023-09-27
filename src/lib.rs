#![feature(impl_trait_in_assoc_type)]
#![feature(associated_type_defaults)]
#![feature(decl_macro)]

pub mod channel;
pub mod serialize;

use channel::{Recving, Sending};
pub use rumpsteak_macros::{choices, session, Message, Role, Roles};

use futures::{FutureExt, Sink, SinkExt, Stream, StreamExt};
use std::{
    any::Any,
    convert::Infallible,
    future::Future,
    marker::{self, PhantomData},
};
use thiserror::Error;

pub type SendError<C, L> = <C as Sending<L>>::Error;

#[derive(Debug, Error)]
pub enum ReceiveError<E> {
    #[error("receiver stream is empty")]
    EmptyStream,
    #[error("received message with an unexpected type")]
    UnexpectedType,
    #[error("receiver error")]
    Error(#[from] E),
}

/// The channel is capable of sending L
pub trait Message<L>: Sized {
    /// Creates a message from a label.
    fn upcast(label: L) -> Self;

    /// Tries to get the label contained in the `Message`. This might fail,
    /// typically if we are trying to get a label of the wrong type. In case of
    /// failure, the result contains `self`, hence the message is not lost.
    fn downcast(self) -> Result<L, Self>;
}

impl<L> Message<L> for L {
    fn downcast(self) -> Result<L, Self> {
        Ok(self)
    }
    fn upcast(label: L) -> Self {
        label
    }
}

pub trait Role {}

pub trait Route<R, Chan>: Role + Sized {
    /// Message type of this channel
    type Onwire;
    fn route(&mut self) -> &mut Chan;
}

/// This structure is mainly a placeholder for a `Role` and for types.
/// Typically, each each state (in the sense of automata state) of the protocol,
/// e.g. a `Send`, a `Receive`, etc, contains a `State`, as well as some type
/// bounds. When an action is taken (e.g. when `send` is called on a `Send`),
/// the `Send` will take it state and convert it into the continuation.
pub struct State<'r, R: Role> {
    role: &'r mut R,
}

impl<'r, R: Role> State<'r, R> {
    #[inline]
    fn new(role: &'r mut R) -> Self {
        Self { role }
    }
}

pub trait FromState<'r> {
    type Role: Role;

    fn from_state(state: State<'r, Self::Role>) -> Self;
}

pub trait Session<'r>: FromState<'r> + private::Session {}

pub trait IntoSession<'r>: FromState<'r> {
    type Session: Session<'r, Role = Self::Role>;

    fn into_session(self) -> Self::Session;
}

/// This structure represents a terminated protocol.
pub struct End<'r, R: Role> {
    _state: State<'r, R>,
}

/// Sel for self role
/// This impl has no effect. Other continues directly.
impl<'k, Sel: Role, Other> PartialDual<'k, Sel, Other> for End<'k, Sel> {
    type Dual<Continuation: FromState<'k, Role = Other>> = Continuation;
}

impl<'r, R: Role> FromState<'r> for End<'r, R> {
    type Role = R;

    #[inline]
    fn from_state(state: State<'r, Self::Role>) -> Self {
        Self { _state: state }
    }
}

impl<'r, R: Role> private::Session for End<'r, R> {}

impl<'r, R: Role> Session<'r> for End<'r, R> {}

pub trait FullDual<'q, A, B>: FromState<'q, Role = A> {
    type Dual: FromState<'q, Role = B>;
}

pub trait PartialDual<'q, A, B>: FromState<'q, Role = A> {
    /// Requires a user supplied continuation because there isn't an obvious one.
    type Dual<Continuation: FromState<'q, Role = B>>: FromState<'q, Role = B>;
}

/// This structure represents a protocol which next action is to send.
pub struct Send<'q, Sender: Role, Receiver, L, S: FromState<'q, Role = Sender>> {
    state: State<'q, Sender>,
    phantom: PhantomData<(Receiver, L, S)>,
}

impl<'q, Q: Role, R: Role + 'q, L, S: FromState<'q, Role = Q> + FullDual<'q, Q, R>> FullDual<'q, Q, R>
    for Send<'q, Q, R, L, S>
where
    <S as FullDual<'q, Q, R>>::Dual: FromState<'q, Role = R>,
{
    type Dual = Receive<'q, R, Q, L, S::Dual>;
}

impl<'q, Q: Role, R: Role + 'q, L, S: FromState<'q, Role = Q>> PartialDual<'q, Q, R>
    for Send<'q, Q, R, L, S>
{
    type Dual<Continuation: FromState<'q, Role = R>> = Receive<'q, R, Q, L, Continuation>;
}

impl<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> FromState<'q> for Send<'q, Q, R, L, S> {
    type Role = Q;

    #[inline]
    fn from_state(state: State<'q, Self::Role>) -> Self {
        Self {
            state,
            phantom: PhantomData,
        }
    }
}

impl<'q, Q: Route<R, C>, R, C, S: FromState<'q, Role = Q>> Send<'q, Q, R, C, S>
where
    C: Sending<<Q as Route<R, C>>::Onwire> + Unpin, 
{
    #[inline]
    pub async fn send<L>(
        self,
        label: L,
    ) -> Result<S, SendError<C, <Q as Route<R, C>>::Onwire>>
    where
        <Q as Route<R, C>>::Onwire: Message<L>,
    {
        self.state.role.route().send(Message::upcast(label)).await?;
        Ok(FromState::from_state(self.state))
    }
}

impl<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> private::Session for Send<'q, Q, R, L, S> {}

impl<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> Session<'q> for Send<'q, Q, R, L, S> {}

/// This structure represents a protocol which next action is to receive .
pub struct Receive<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> {
    state: State<'q, Q>,
    phantom: PhantomData<(R, L, S)>,
}

impl<'q, Q: Role, R: Role + 'q, L, S: FromState<'q, Role = Q> + FullDual<'q, Q, R>> FullDual<'q, Q, R>
    for Receive<'q, Q, R, L, S>
where
    <S as FullDual<'q, Q, R>>::Dual: FromState<'q, Role = R>,
{
    type Dual = Send<'q, R, Q, L, S::Dual>;
}

impl<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> FromState<'q> for Receive<'q, Q, R, L, S> {
    type Role = Q;

    #[inline]
    fn from_state(state: State<'q, Self::Role>) -> Self {
        Self {
            state,
            phantom: PhantomData,
        }
    }
}

impl<'q, R: Route<S, C>, S: Role, C, N: FromState<'q, Role = R>> Receive<'q, R, S, C, N>
where
    C: Recving<<R as Route<S, C>>::Onwire> + Unpin,
{
    #[inline]
    pub async fn receive<L>(
        self,
    ) -> Result<(L, N), ReceiveError<<C as Recving<<R as Route<S, C>>::Onwire>>::Error>>
    where
        <R as Route<S, C>>::Onwire: Message<L>,
    {
        let message = self.state.role.route().recv().await?;
        let message = message.ok_or(ReceiveError::EmptyStream)?;
        Ok((
            Message::downcast(message).or(Err(ReceiveError::UnexpectedType))?,
            FromState::from_state(self.state),
        ))
    }
}

impl<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> private::Session for Receive<'q, Q, R, L, S> {}

impl<'q, Q: Role, R, L, S: FromState<'q, Role = Q>> Session<'q> for Receive<'q, Q, R, L, S> {}

pub trait Choice<'r, Brancher: Role> {
    type Selector: Role;
    // type Brancher: Role;
    type SelectorSession: FromState<'r, Role = Self::Selector>;
    type BrancherSession: FromState<'r, Role = Brancher>;
}

/// Less flexible but convenient. Implementing [ChoiceB] derives [Choice]
pub trait ChoiceB<'r> {
    type Selector: Role;
    type Brancher: Role;
    type SelectorSession: FromState<'r, Role = Self::Selector>;
    type BrancherSession: FromState<'r, Role = Self::Brancher>;
}

impl<'r, C: ChoiceB<'r>> Choice<'r, C::Brancher> for C {
    type BrancherSession = <Self as ChoiceB<'r>>::BrancherSession;
    type Selector = <Self as ChoiceB<'r>>::Selector;
    type SelectorSession = <Self as ChoiceB<'r>>::SelectorSession;
}

pub trait ChoiceV<E> {
    fn v(self) -> E;
}

pub struct Select<'q, Sel: Role, Br, C> {
    state: State<'q, Sel>,
    phantom: PhantomData<(Br, C)>,
}

impl<'q, S: Role, B: Role + 'q, C> FullDual<'q, S, B> for Select<'q, S, B, C> {
    type Dual = Branch<'q, B, S, C>;
}

impl<'q, Q: Role, R, C> FromState<'q> for Select<'q, Q, R, C> {
    type Role = Q;

    #[inline]
    fn from_state(state: State<'q, Self::Role>) -> Self {
        Self {
            state,
            phantom: PhantomData,
        }
    }
}

impl<'q, Chan, Sel: Route<Br, Chan>, Br: Role> Select<'q, Sel, Br, Chan> {
    #[inline]
    pub async fn select<C: Choices, V: ChoiceV<C::Repr> + Choice<'q, Br, Selector = Sel>>(
        self,
        label: V,
    ) -> Result<
        <V as Choice<'q, Br>>::SelectorSession,
        SendError<Chan, <Sel as Route<Br, Chan>>::Onwire>,
    >
    where
        Chan: Sending<<Sel as Route<Br, Chan>>::Onwire> + Unpin,
        <Sel as Route<Br, Chan>>::Onwire: Message<C::Repr>,
    {
        let k = Message::upcast(label.v());
        self.state.role.route().send(k).await?;
        Ok(FromState::from_state(self.state))
    }
}

impl<'q, Q: Role, R, C> private::Session for Select<'q, Q, R, C> {}

impl<'q, Q: Role, R, C> Session<'q> for Select<'q, Q, R, C> {}

pub trait Choices: Sized {
    /// The enum
    type Repr;
}

pub struct Branch<'q, Brancher: Role, Selector, BranchUnit> {
    state: State<'q, Brancher>,
    phantom: PhantomData<(Selector, BranchUnit)>,
}

impl<'q, Q: Role, R, C> FromState<'q> for Branch<'q, Q, R, C> {
    type Role = Q;

    #[inline]
    fn from_state(state: State<'q, Self::Role>) -> Self {
        Self {
            state,
            phantom: PhantomData,
        }
    }
}

impl<'q, Chan, Br: Route<Sel, Chan>, Sel: Role + 'q> FullDual<'q, Br, Sel> for Branch<'q, Br, Sel, Chan>
where
    Chan: Recving<<Sel as Route<Br, Chan>>::Onwire> + Unpin,
    Sel: Route<Br, Chan>,
{
    type Dual = Select<'q, Sel, Br, Chan>;
}

impl<'q, Chan, Br: Role + Route<Br, Chan>, Sel: Role + Route<Br, Chan>> Branch<'q, Br, Sel, Chan>
where
    Chan: Recving<<Sel as Route<Br, Chan>>::Onwire> + Unpin,
{
    /// Returns the enum
    #[inline]
    pub async fn branch<C: Choices>(
        &mut self,
    ) -> Result<C::Repr, ReceiveError<<Chan as Recving<<Sel as Route<Br, Chan>>::Onwire>>::Error>>
    where
        <Sel as Route<Br, Chan>>::Onwire: Message<C::Repr>,
    {
        let message = self.state.role.route().recv().await?;
        let message = message.ok_or(ReceiveError::EmptyStream)?;
        let message = Message::downcast(message).or(Err(ReceiveError::UnexpectedType))?;
        Ok(message)
    }
    pub fn next<B: Choice<'q, Br, Selector = Sel>>(self, _typ: &B) -> B::BrancherSession {
        FromState::from_state(self.state)
    }
}

impl<'q, Q: Role, R, C> private::Session for Branch<'q, Q, R, C> {}

impl<'q, Q: Role, R, C> Session<'q> for Branch<'q, Q, R, C> {}

#[inline]
pub async fn session<'r, R: Role, S: FromState<'r, Role = R>, T, F>(
    role: &'r mut R,
    f: impl FnOnce(S) -> F,
) -> T
where
    F: Future<Output = (T, End<'r, R>)>,
{
    let output = try_session(role, |s| f(s).map(Ok)).await;
    output.unwrap_or_else(|infallible: Infallible| match infallible {})
}

#[inline]
pub async fn try_session<'r, R: Role, S: FromState<'r, Role = R>, T, E, F>(
    role: &'r mut R,
    f: impl FnOnce(S) -> F,
) -> Result<T, E>
where
    F: Future<Output = Result<(T, End<'r, R>), E>>,
{
    let session = FromState::from_state(State::new(role));
    f(session).await.map(|(output, _)| output)
}

#[inline]
pub fn try_session_sync<'r, R: Role, S: FromState<'r, Role = R>, T, E, F>(
    role: &'r mut R,
    f: impl FnOnce(S) -> Result<(T, End<'r, R>), E>,
) -> Result<T, E> {
    let session = FromState::from_state(State::new(role));
    f(session).map(|(output, _)| output)
}

mod private {
    pub trait Session {}
}

// Implement it on variants of an enum.
// pub macro choiceb( $id:ident, $li:lifetime, $s:ty => $se:ty, $b:ty => $br:ty  ) {
//     impl<$li> ChoiceB<$li> for $id {
//         type Brancher = $b;
//         type Selector = $s;
//         #[session($li $s)]
//         type SelectorSession = $se;
//         #[session($li $b)]
//         type BrancherSession = $br;
//     }
// }
