//! Effectful versions of standard higher-order functions.

use {
    crate::{injection::EffectList, Effectful, EffectfulState},
    core::pin::Pin,
};

/// An implementation detail of [`OptionExt::map_eff`].
#[derive(Clone)]
pub struct OptionMapEff<G> {
    inner: Option<G>,
}

impl<G> Effectful for OptionMapEff<G>
where
    G: Effectful,
{
    type Effects = G::Effects;
    type Output = Option<G::Output>;

    fn resume(
        self: Pin<&mut Self>,
        injections: <Self::Effects as EffectList>::Injections,
    ) -> EffectfulState<Self::Effects, Self::Output> {
        unsafe {
            match &mut self.get_unchecked_mut().inner {
                Some(g) => match Pin::new_unchecked(g).resume(injections) {
                    EffectfulState::Perform(effects) => EffectfulState::Perform(effects),
                    EffectfulState::Return(output) => EffectfulState::Return(Some(output)),
                },
                None => EffectfulState::Return(None),
            }
        }
    }
}

/// Higher-order effectful functions on [`Option`].
pub trait OptionExt<T>: Sized {
    /// Transforms the value inside an [`Option::Some`] with some effects.
    ///
    /// This function is analogous to [`Option::map`] except it allows its argument to have effects
    /// which must then be handled by its caller. This means that the mapper function can, for
    /// example, await a `Future`. Other control flow constructs are of course possible here, and
    /// impossible with vanilla [`Option::map`].
    fn map_eff<G>(self, g: impl FnOnce(T) -> G) -> OptionMapEff<G>
    where
        G: Effectful;
}

impl<T> OptionExt<T> for Option<T> {
    fn map_eff<G>(self, g: impl FnOnce(T) -> G) -> OptionMapEff<G>
    where
        G: Effectful,
    {
        OptionMapEff { inner: self.map(g) }
    }
}

/// An implementation detail of [`ResultExt::map_eff`].
#[derive(Clone)]
pub struct ResultMapEff<G, E> {
    inner: Option<Result<G, E>>,
}

impl<G, E> Effectful for ResultMapEff<G, E>
where
    G: Effectful,
    E: Unpin,
{
    type Effects = G::Effects;
    type Output = Result<G::Output, E>;

    fn resume(
        self: Pin<&mut Self>,
        injections: <Self::Effects as EffectList>::Injections,
    ) -> EffectfulState<Self::Effects, Self::Output> {
        unsafe {
            let g = &mut self.get_unchecked_mut().inner;
            match g {
                Some(Ok(g)) => match Pin::new_unchecked(g).resume(injections) {
                    EffectfulState::Perform(effects) => EffectfulState::Perform(effects),
                    EffectfulState::Return(output) => EffectfulState::Return(Ok(output)),
                },
                Some(Err(_)) => {
                    let Some(Err(e)) = core::mem::take(g) else { unreachable!() };
                    EffectfulState::Return(Err(e))
                },
                None => panic!("resumed after completed"),
            }
        }
    }
}

/// An implementation detail of [`ResultExt::map_err_eff`].
#[derive(Clone)]
pub struct ResultMapErrEff<T, G> {
    inner: Option<Result<T, G>>,
}

impl<T, G> Effectful for ResultMapErrEff<T, G>
where
    T: Unpin,
    G: Effectful,
{
    type Effects = G::Effects;
    type Output = Result<T, G::Output>;

    fn resume(
        self: Pin<&mut Self>,
        injections: <Self::Effects as EffectList>::Injections,
    ) -> EffectfulState<Self::Effects, Self::Output> {
        unsafe {
            let g = &mut self.get_unchecked_mut().inner;
            match g {
                Some(Err(g)) => match Pin::new_unchecked(g).resume(injections) {
                    EffectfulState::Perform(effects) => EffectfulState::Perform(effects),
                    EffectfulState::Return(output) => EffectfulState::Return(Err(output)),
                },
                Some(Ok(_)) => {
                    let Some(Ok(e)) = core::mem::take(g) else { unreachable!() };
                    EffectfulState::Return(Ok(e))
                },
                None => panic!("resumed after completed"),
            }
        }
    }
}

/// Higher-order effectful functions on [`Result`].
pub trait ResultExt<T, E>: Sized {
    /// Transforms the value inside a [`Result::Ok`] with some effects.
    ///
    /// For more details, see [`OptionExt::map_eff`].
    fn map_eff<G>(self, g: impl FnOnce(T) -> G) -> ResultMapEff<G, E>
    where
        G: Effectful;

    /// Transforms the value inside a [`Result::Err`] with some effects.
    ///
    /// For more details, see [`OptionExt::map_eff`].
    fn map_err_eff<G>(self, g: impl FnOnce(E) -> G) -> ResultMapErrEff<T, G>
    where
        G: Effectful;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    fn map_eff<G>(self, g: impl FnOnce(T) -> G) -> ResultMapEff<G, E>
    where
        G: Effectful,
    {
        ResultMapEff {
            inner: Some(self.map(g)),
        }
    }

    fn map_err_eff<G>(self, g: impl FnOnce(E) -> G) -> ResultMapErrEff<T, G>
    where
        G: Effectful,
    {
        ResultMapErrEff {
            inner: Some(self.map_err(g)),
        }
    }
}
