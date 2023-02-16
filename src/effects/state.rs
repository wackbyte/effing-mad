use {
    crate::{
        data::Union, injection::EffectList, lift, perform, Effect, EffectGroup, Effectful,
        GeneratorToEffectful,
    },
    core::{
        fmt::{self, Debug, Formatter},
        marker::PhantomData,
    },
};

pub struct Get<T>(PhantomData<fn() -> T>);

impl<T> Copy for Get<T> {}

impl<T> Clone for Get<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Debug for Get<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Get").finish()
    }
}

impl<T> Default for Get<T> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T> Effect for Get<T> {
    type Injection = T;
}

#[derive(Copy, Clone, Debug, Default)]
pub struct Set<T>(pub T);

impl<T> Effect for Set<T> {
    type Injection = ();
}

pub struct State<T>(PhantomData<T>);

impl<T> EffectGroup for State<T> {
    type Effects = Union!(Get<T>, Set<T>);
}

type StateEffects<T> = <State<T> as EffectGroup>::Effects;
type StateInjections<T> = <StateEffects<T> as EffectList>::Injections;

impl<T> State<T> {
    pub const fn get() -> Get<T> {
        Get(PhantomData)
    }

    pub const fn set(value: T) -> Set<T> {
        Set(value)
    }

    pub fn with(f: impl FnOnce(T) -> T) -> impl Effectful<Effects = StateEffects<T>> {
        GeneratorToEffectful::new(move |_begin: StateInjections<T>| {
            let state = perform!(Self::get());
            let state = f(state);
            perform!(Self::set(state));
        })
    }

    pub fn with_mut(f: impl FnOnce(&mut T)) -> impl Effectful<Effects = StateEffects<T>> {
        GeneratorToEffectful::new(move |_begin: StateInjections<T>| {
            let mut state = perform!(Self::get());
            f(&mut state);
            perform!(Self::set(state));
        })
    }
}
