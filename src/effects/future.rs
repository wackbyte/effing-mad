//! Functions and types for converting between Futures and effectful computations.

use {
    crate::{
        data::Union,
        injection::{Begin, EffectList, Tagged},
        Effect, EffectGroup, Effectful, EffectfulState,
    },
    core::{
        future::Future,
        pin::Pin,
        task::{Context, Poll, Waker},
    },
};

/// An effect equivalent to `future.await`.
pub struct Await;

impl Effect for Await {
    type Injection = ();
}

/// A request that the handler provide a [Context](core::task::Context) so that futures can be
/// polled.
pub struct GetContext;

impl Effect for GetContext {
    // no lifetimes issues now lmao
    type Injection = *const Waker;
}

/// The effects that allow effectful computations to emulate Futures.
pub struct Async;

impl EffectGroup for Async {
    type Effects = Union!(Await, GetContext);
}

type AsyncEffects = <Async as EffectGroup>::Effects;
type AsyncInjections = <AsyncEffects as EffectList>::Injections;

/// Brings a computation from effects land into futures land.
///
/// If a computation has effects [`GetContext`] and [`Await`] then it behaves like a Future and so
/// can be used as one through this wrapper. See [`EffExt::futurise`] for a way to construct this.
pub struct EffectfulToFuture<G> {
    inner: G,
    // TODO: if we specify that the first inj doesn't *have* to be `Begin`, we can get rid of this
    // field. would that work?
    has_begun: bool,
}

impl<G> Future for EffectfulToFuture<G>
where
    G: Effectful<Effects = AsyncEffects>,
{
    type Output = <G as Effectful>::Output;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // see the todo on has_begun
        let mut injections = if self.has_begun {
            Union::inject(Tagged::new(()))
        } else {
            Union::inject(Begin)
        };

        // project
        let pin = unsafe { Pin::new_unchecked(&mut self.as_mut().get_unchecked_mut().inner) };
        let state = pin.resume(injections);
        match state {
            EffectfulState::Perform(effects) => match effects.uninject() {
                Ok(GetContext) => {
                    injections = Union::inject(Tagged::new(cx.waker() as *const Waker))
                },
                Err(Union::Inl(Await)) => panic!("can't Await without context"),
                Err(Union::Inr(never)) => match never {},
            },
            EffectfulState::Return(output) => return Poll::Ready(output),
        }

        let pin = unsafe { Pin::new_unchecked(&mut self.get_unchecked_mut().inner) };
        let state = pin.resume(injections);
        match state {
            EffectfulState::Perform(effects) => match effects.uninject() {
                Ok(Await) => Poll::Pending,
                Err(Union::Inl(GetContext)) => panic!("no need to GetContext twice"),
                Err(Union::Inr(never)) => match never {},
            },
            EffectfulState::Return(output) => Poll::Ready(output),
        }
    }
}

/// Brings a future into effects land.
///
/// Futures can be expressed in terms of effectful computations with effects [`GetContext`] and
/// [`Await`]. See [`FutureExt::effectfulise`] for a way to construct this.
pub struct FutureToEffectful<F> {
    inner: F,
}

impl<F> Effectful for FutureToEffectful<F>
where
    F: Future,
{
    type Effects = AsyncEffects;
    type Output = F::Output;

    fn resume(
        self: Pin<&mut Self>,
        injections: AsyncInjections,
    ) -> EffectfulState<Self::Effects, Self::Output> {
        // safety: project
        let pin = unsafe { self.map_unchecked_mut(|s| &mut s.inner) };
        match injections.uninject::<Tagged<*const Waker, _>, _>() {
            // safety: it's a pointer to something that the caller has a reference to.
            Ok(waker) => match pin.poll(&mut Context::from_waker(unsafe { &*waker.untag() })) {
                Poll::Ready(output) => EffectfulState::Return(output),
                Poll::Pending => EffectfulState::Perform(Union::inject(GetContext)),
            },
            Err(_) => EffectfulState::Perform(Union::inject(GetContext)),
        }
    }
}

/// Future-related functions on effectful computations.
pub trait EffectfulExt: Sized {
    /// Wraps this computation in a type that allows it to be used as a `Future`.
    fn to_future(self) -> EffectfulToFuture<Self>;
}

impl<G> EffectfulExt for G
where
    G: Effectful<Effects = AsyncEffects>,
{
    fn to_future(self) -> EffectfulToFuture<Self> {
        EffectfulToFuture {
            inner: self,
            has_begun: false,
        }
    }
}

/// Effect-related functions on [Future](core::future::Future)s.
pub trait FutureExt: Sized {
    /// Wraps this `Future` in a type that allows it to be used as an effectful computation.
    fn to_effectful(self) -> FutureToEffectful<Self>;
}

impl<F> FutureExt for F
where
    F: Future,
{
    fn to_effectful(self) -> FutureToEffectful<Self> {
        FutureToEffectful { inner: self }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::{EffectfulExt, FutureExt},
        std::{
            future::Future,
            sync::Arc,
            task::{Context, Poll, Wake},
        },
    };

    struct NoopWake;
    impl Wake for NoopWake {
        fn wake(self: Arc<Self>) {}
    }

    #[test]
    fn back_and_forth() {
        // lol
        let f = std::future::ready(5);
        let eff_f = f.to_effectful();
        let fut_eff_f = eff_f.to_future();
        let eff_fut_eff_f = fut_eff_f.to_effectful();
        let fut_eff_fut_eff_f = eff_fut_eff_f.to_future();
        let pin_fut_eff_fut_eff_f = std::pin::pin!(fut_eff_fut_eff_f);

        let waker = Arc::new(NoopWake).into();
        let mut context = Context::from_waker(&waker);
        assert_eq!(pin_fut_eff_fut_eff_f.poll(&mut context), Poll::Ready(5))
    }
}
