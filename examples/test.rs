use rumpsteak::channel::Recving;
use rumpsteak::session;

use rumpsteak::*;
use static_assertions::*;

#[derive(Role)]
struct R(#[route(R, M)] C);

#[session]
type S3 = Send<R, R, Send<R, R, Send<R, R, End>>>;

#[session]
type R3 = Receive<R, R, Receive<R, R, Receive<R, R, End>>>;

#[session]
type S2R3 = Send<R, R, Send<R, R, Receive<R, R, Receive<R, R, Receive<R, R, End>>>>>;

#[session]
type B1 = Send<R, R, Send<R, R, Select<R, (C, B)>>>;

#[session]
type B2 = Receive<R, R, Receive<R, R, Branch<R, (C, B)>>>;

struct C;
struct M;

#[choices]
pub enum B {}


#[test]
fn pdual() {
    let mut p = R(C);
    let k: <S3<'_, R> as PartialDual<'_, R, R>>::Dual<End<R>> =
        FromState::from_state(State::new(&mut p));
    let mut b = k;
    let k: <S3<'_, R> as FullDual<'_, R, R>>::Dual = FromState::from_state(State::new(&mut p));
    b = k;

    assert_type_eq_all!(
        <S3<'_, R> as PartialDual<'_, R, R>>::Dual<End<R>>,
        R3<'_, R>,
        <S3<'_, R> as FullDual<'_, R, R>>::Dual
    );
    let k: <S2R3<'_, R> as PartialDual<'_, R, R>>::Dual<End<R>> =
        FromState::from_state(State::new(&mut p));
    let b = k;

    let k: B1<'_, R> = FromState::from_state(State::new(&mut p));
    let b = k;

    let k: <B1<'_, R> as FullDual<R, R> >::Dual = FromState::from_state(State::new(&mut p));
    let b = k;

    let k: <B2<'_, R> as FullDual<R, R> >::Dual = FromState::from_state(State::new(&mut p));
    let b = k;

    assert_type_eq_all!(<B1<'_, R> as FullDual<R, R> >::Dual, B2<'_, R>);
    assert_type_eq_all!(<B2<'_, R> as FullDual<R, R> >::Dual, B1<'_, R>);
}
