use anyhow::Result;
use futures::{
    channel::{mpsc, oneshot::channel},
    executor, try_join,
};
use rumpsteak::{
    channel::Bidirectional, choiceb, session, try_session, Branch, Choice, ChoiceB, ChoiceV,
    Choices, FullDual, End, Message, Receive, Role, Roles, Route, Select, Send,
};
use rumpsteak_macros::choices;

type Sender = mpsc::UnboundedSender<Label>;
type Receiver = mpsc::UnboundedReceiver<Label>;

#[derive(Roles)]
struct Roles(A, B);

#[derive(Role)]
#[message(Label)]
struct A(#[route(B)] Bidirectional<Sender, Receiver>);

#[derive(Role)]
#[message(Label)]
struct B(#[route(A)] Bidirectional<Sender, Receiver>);

#[derive(Message, Debug)]
enum Label {
    Add(Add),
    Sub(Sub),
    RingAC(RingAChoice),
}

#[derive(Debug)]
struct Add(i32);

#[derive(Debug)]
struct Sub(i32);

#[session]
type RingA = Send<B, Add, Branch<B, RingAChoiceBranch>>;

#[session]
struct Wrapped(Send<B, Add, Receive<B, Sub, End>>);

#[choices]
#[derive(Debug)]
enum RingAChoice {
    CAdd,
    CEnd,
}

choiceb! {
    CEnd, 'k,
    B => End, A => End
}

choiceb! {
    CAdd, 'k,
    B => <Self::BrancherSession as Dual<'k, A, B>>::Dual,
    A => RingA
}

async fn ring_a(role: &mut A, mut input: i32) -> Result<()> {
    try_session(role, |mut s: RingA<'_, A>| async {
        loop {
            let mut sent = s.send(Add(2)).await?;
            match sent.branch().await? {
                RingAChoice::CAdd(_t) => {
                    s = sent.next(_t);
                }
                RingAChoice::CEnd(_t) => {
                    break Ok(((), sent.next(_t)));
                }
            }
        }
    })
    .await
}

async fn ring_b(role: &mut B, mut input: i32) -> Result<()> {
    try_session(
        role,
        |mut s: <RingA<'_, A> as FullDual<'_, A, B>>::Dual| async {
            loop {
                let (add, select) = s.receive().await?;
                dbg!(&add);
                input += add.0;
                s = if input > 5 {
                    break Ok(((), select.select(CEnd).await?));
                } else {
                    select.select(CAdd).await?
                }
            }
        },
    )
    .await
}

fn main() {
    let Roles(mut a, mut b) = Roles::default();
    executor::block_on(async {
        try_join!(ring_a(&mut a, -1), ring_b(&mut b, 0)).unwrap();
    });
}
