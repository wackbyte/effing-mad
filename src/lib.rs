//! This library brings typed effects to Rust in a flexible and composable way. By building on
//! [`Generator`]s, effectful computations can be expressed in a way that allows arbitrary and
//! swappable behaviour - any handler of the correct type can be applied to a computation, meaning
//! different semantics of effects can be selected at each call site of an effectful function.
//!
//! ## Glossary
//! - effectful computation: an in-progress computation that uses effects. analogous to `Future`.
//! - effectful function: a function that returns an effectful computation. analogous to `async fn`.
//! - effect: you know, I'm actually not sure. I should ask one of my PL teachers. In this library
//! though, an effect is a value that can be passed out of an effectful computation and into an
//! effect handler, which produces another value to pass back in.
//! - injection: an `effing_mad` term referring to the value passed into a computation as a result of
//! it running an effect.
//! - "pure" function: a Rust function that does not use `effing_mad` effects. Rust is not a pure
//! language (crudely, Rust code can `println!()` whenever it wants) so these docs use quotes to
//! indicate this meaning as opposed to the real meaning of pure, where functions do not use side
//! effects.
//!
//! ## Getting started
//! Define an [`Effect`]. Now, you can define an [`#[effectful(…)]`](effing_macros::effectful)
//! function that uses it. Once you call this function, its effects can be handled one by one with
//! [`handle`]. Handlers are "pure" Rust functions, but it's easiest to construct them using
//! [`handler!`](effing_macros::handler). Once all the effects have been handled away, a computation
//! can be driven with [`run`].

#![feature(decl_macro)]
#![feature(doc_auto_cfg)]
#![feature(doc_notable_trait)]
#![feature(generators)]
#![feature(generator_trait)]
#![feature(never_type)]
#![cfg_attr(not(any(test, feature = "std")), no_std)]
#![warn(missing_docs)]

extern crate alloc;

pub mod data;
pub mod effects;
//pub mod higher_order;
pub mod injection;

#[doc(hidden)]
pub mod macro_impl;

pub use self::macro_impl::{lift, perform};
pub use effing_macros::{effectful, handler};

use {
    self::{
        data::{
            union::{self, Inject, Subset, Superset, Uninject},
            Union, Void,
        },
        injection::{Begin, EffectList, Tagged},
    },
    core::{
        future::Future,
        marker::{PhantomData, Unpin},
        ops::{ControlFlow, Generator, GeneratorState},
        pin::{pin, Pin},
    },
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum EffectfulState<G>
where
    G: Effectful,
{
    Perform(G::Effects, G),
    Return(G::Output),
}

pub trait Effectful {
    type Effects: EffectList;
    type Output;

    fn resume(self, injections: <Self::Effects as EffectList>::Injections) -> EffectfulState<Self>
    where
        Self: Sized;
}

pub struct GeneratorToEffectful<G, Es> {
    inner: G,
    marker: PhantomData<*mut Es>,
}

impl<G, Es> GeneratorToEffectful<G, Es> {
    pub const fn new(inner: G) -> Self {
        Self {
            inner,
            marker: PhantomData,
        }
    }
}

impl<G, Es> Copy for GeneratorToEffectful<G, Es> where G: Copy {}

impl<G, Es> Clone for GeneratorToEffectful<G, Es>
where
    G: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            marker: PhantomData,
        }
    }
}

impl<G, Es> Effectful for GeneratorToEffectful<G, Es>
where
    G: Generator<Es::Injections, Yield = Es> + Unpin,
    Es: EffectList,
{
    type Effects = Es;
    type Output = G::Return;

    fn resume(
        mut self,
        injections: <Self::Effects as EffectList>::Injections,
    ) -> EffectfulState<Self> {
        let state = {
            let pin = Pin::new(&mut self.inner);
            pin.resume(injections)
        };
        match state {
            GeneratorState::Yielded(effects) => EffectfulState::Perform(effects, self),
            GeneratorState::Complete(output) => EffectfulState::Return(output),
        }
    }
}

/*
#[derive(Clone)]
pub struct EffectfulToGenerator<G> {
    inner: G,
}

impl<G> EffectfulToGenerator<G> {
    pub const fn new(inner: G) -> Self {
        Self { inner }
    }
}

impl<G> Generator<<G::Effects as EffectList>::Injections> for EffectfulToGenerator<G>
where
    G: Effectful,
{
    type Yield = G::Effects;
    type Return = G::Output;

    fn resume(
        self: Pin<&mut Self>,
        injections: <G::Effects as EffectList>::Injections,
    ) -> GeneratorState<Self::Yield, Self::Return> {
        let pin = unsafe { self.map_unchecked_mut(|s| &mut s.inner) };
        match Effectful::resume(pin, injections) {
            EffectfulState::Perform(effects) => GeneratorState::Yielded(effects),
            EffectfulState::Return(output) => GeneratorState::Complete(output),
        }
    }
}
*/
pub trait IntoEffect {
    type Injection;
    type IntoEffect: Effect<Injection = Self::Injection>;

    fn into_effect(self) -> Self::IntoEffect;
}

impl<E> IntoEffect for E
where
    E: Effect,
{
    type Injection = E::Injection;
    type IntoEffect = E;

    fn into_effect(self) -> Self::IntoEffect {
        self
    }
}

pub trait IntoEffectful {
    type Effects: EffectList;
    type Output;
    type IntoEffectful: Effectful<Effects = Self::Effects, Output = Self::Output>;

    fn into_effectful(self) -> Self::IntoEffectful;
}

impl<G> IntoEffectful for G
where
    G: Effectful,
{
    type Effects = G::Effects;
    type Output = G::Output;
    type IntoEffectful = Self;

    fn into_effectful(self) -> Self::IntoEffectful {
        self
    }
}

/// An effect that must be handled by the caller of an effectful computation, or propagated up the
/// call stack.
pub trait Effect {
    /// The type of value that running this effect gives.
    type Injection;
}

/// Types which represent multiple effects.
///
/// Effects that are commonly used together can be grouped using a type that implements this trait.
/// Defining an [`#[effectful]`](effing_macros::effectful) function that uses all of the effects in
/// a group can then be done by naming the group instead of each effect.
#[doc(notable_trait)]
pub trait EffectGroup {
    /// A [`Coproduct`](frunk::coproduct::Coproduct) of effects in this group.
    type Effects: EffectList;
}

impl<E> EffectGroup for E
where
    E: Effect,
{
    type Effects = Union!(E);
}

#[derive(Copy, Clone, Debug)]
pub struct Ready<T> {
    value: T,
}

impl<T> Effectful for Ready<T> {
    type Effects = Void;
    type Output = T;

    fn resume(
        self,
        _injections: <Self::Effects as EffectList>::Injections,
    ) -> EffectfulState<Self> {
        EffectfulState::Return(self.value)
    }
}

/// Run an effectful computation that has no effects.
///
/// Effectful computations are generators, but if they have no effects, it is guaranteed that they
/// will never yield. Therefore they can be run by resuming them once. This function does that.
pub fn run<R>(mut g: impl Effectful<Effects = Void, Output = R>) -> R {
    match g.resume(Union::Inl(Begin)) {
        EffectfulState::Perform(never, _) => match never {},
        EffectfulState::Return(output) => output,
    }
}

pub struct Mapp<G, F, O> {
    g: G,
    f: F,
    marker: PhantomData<fn() -> O>,
}

impl<G, F, O> Effectful for Mapp<G, F, O>
where
    G: Effectful,
    F: FnOnce(G::Output) -> O,
{
    type Effects = G::Effects;
    type Output = O;

    fn resume(self, injections: <Self::Effects as EffectList>::Injections) -> EffectfulState<Self> {
        match self.g.resume(injections) {
            EffectfulState::Perform(effects, g) => {
                EffectfulState::Perform(effects, Self { g, ..self })
            },
            EffectfulState::Return(output) => EffectfulState::Return((self.f)(output)),
        }
    }
}

impl<G, F, O> Clone for Mapp<G, F, O>
where
    G: Clone,
    F: Clone,
{
    fn clone(&self) -> Self {
        Self {
            g: self.g.clone(),
            f: self.f.clone(),
            marker: PhantomData,
        }
    }
}

/// Create a new effectful computation by applying a "pure" function to the return value of an
/// existing computation.
pub fn map<G, F, O>(g: G, f: F) -> Mapp<G, F, O> {
    Mapp {
        g,
        f,
        marker: PhantomData,
    }
}
/*
/// Apply a "pure" handler to an effectful computation, handling one effect.
///
/// When given an effectful computation with effects (A, B, C) and a handler for effect C, this
/// returns a new effectful computation with effects (A, B). Handlers can choose for each instance
/// of their effect whether to resume the computation, passing in a value (injection) or to force a
/// return from the computation. This is done using
/// [`ControlFlow::Continue`](core::ops::ControlFlow::Continue) and
/// [`ControlFlow::Break`](core::ops::ControlFlow::Break) respectively.
///
/// For handling multiple effects with one closure, see [`handle_group`].
pub fn handle<
    R,
    E,
    PreEs,
    PostEs,
    EffectIndex,
    BeginIndex,
    InjectionIndex,
    InjectionIndices,
    SupersetIndices,
>(
    g: impl Effectful<Effects = PreEs, Output = R>,
    mut handler: impl FnMut(E) -> ControlFlow<R, E::Injection>,
) -> impl Effectful<Effects = PostEs, Output = R>
where
    E: Effect,
    PreEs: EffectList + Uninject<E, EffectIndex, Remainder = PostEs>,
    PostEs: EffectList,
    Union!(Tagged<E::Injection, E>, Begin): Superset<PreEs::Injections, InjectionIndices>,
    PreEs::Injections: Inject<Begin, BeginIndex> + Inject<Tagged<E::Injection, E>, InjectionIndex>,
    PostEs::Injections: Superset<PreEs::Injections, SupersetIndices>,
{
    handle_group(g, move |effects| match effects {
        Union::Inl(effect) => match handler(effect) {
            ControlFlow::Continue(injection) => {
                ControlFlow::Continue(Union::Inl(Tagged::new(injection)))
            },
            ControlFlow::Break(output) => ControlFlow::Break(output),
        },
        Union::Inr(never) => match never {},
    })
}

/// Apply a "pure" handler to an effectful computation, handling any number of effects.
///
/// When given an effectful computation with effects (A, B, C, D) and a handler for effects (A, B),
/// this function returns a new effectful computation with effects (C, D). Handlers can choose for
/// each instance of their effects whether to resume the computation, passing in a value (injection)
/// or to force a return from the computation. This is done using
/// [`ControlFlow::Continue`](core::ops::ControlFlow::Continue) and
/// [`ControlFlow::Break`](core::ops::ControlFlow::Break) respectively.
///
/// `Es` must be a [`Coproduct`](frunk::Coproduct) of effects.
///
/// Care should be taken to only produce an injection type when handling the corresponding effect.
/// If the injection type does not match the effect that is being handled, the computation will
/// most likely panic.
pub fn handle_group<
    R,
    Es,
    PreEs,
    PostEs,
    EffectIndices,
    BeginIndex,
    InjectionIndices,
    SupersetIndices,
>(
    mut g: impl Effectful<Effects = PreEs, Output = R>,
    mut handler: impl FnMut(Es) -> ControlFlow<R, Es::Injections>,
) -> impl Effectful<Effects = PostEs, Output = R>
where
    Es: EffectList,
    PreEs: EffectList + Subset<Es, EffectIndices, Remainder = PostEs>,
    PostEs: EffectList,
    Es::Injections: Superset<PreEs::Injections, InjectionIndices>,
    PreEs::Injections: Inject<Begin, BeginIndex>,
    PostEs::Injections: Superset<PreEs::Injections, SupersetIndices>,
{
    GeneratorToEffectful::new(move |_begin: PostEs::Injections| {
        let mut injections = PreEs::Injections::inject(Begin);
        loop {
            // safety: im 90% sure that since we are inside Generator::resume which takes
            // Pin<&mut self>, all locals in this function are effectively pinned and this call is
            // simply projecting that
            let pin = unsafe { Pin::new_unchecked(&mut g) };
            match pin.resume(injections) {
                EffectfulState::Perform(effects) => match effects.subset() {
                    Ok(effects) => match handler(effects) {
                        ControlFlow::Continue(new_injections) => {
                            injections = new_injections.superset()
                        },
                        ControlFlow::Break(ret) => return ret,
                    },
                    Err(effects) => injections = (yield effects).superset(),
                },
                EffectfulState::Return(output) => return output,
            }
        }
    })
}
*/

pub struct HandleGroup<
    G,
    H,
    HandledEs,
    PostEs,
    EffectIndices,
    InjectionIndices,
    SupersetIndices,
> {
    g: G,
    h: H,
    marker: PhantomData<*mut (
        HandledEs,
        PostEs,
        EffectIndices,
        InjectionIndices,
        SupersetIndices,
    )>,
}

impl<G, H, HandledEs, PostEs, EffectIndices, InjectionIndices, SupersetIndices>
    Effectful
    for HandleGroup<
        G,
        H,
        HandledEs,
        PostEs,
        EffectIndices,
        InjectionIndices,
        SupersetIndices,
    >
where
    G: Effectful,
    H: FnMut(HandledEs) -> ControlFlow<G::Output, <HandledEs as EffectList>::Injections>,
    HandledEs: EffectList,
    G::Effects: Subset<HandledEs, EffectIndices, Remainder = PostEs>,
    PostEs: EffectList,
    HandledEs::Injections: Superset<<G::Effects as EffectList>::Injections, InjectionIndices>,
    PostEs::Injections: Superset<<G::Effects as EffectList>::Injections, SupersetIndices>,
{
    type Effects = PostEs;
    type Output = G::Output;

    fn resume(self, injections: <Self::Effects as EffectList>::Injections) -> EffectfulState<Self> {
        let Self { mut g, mut h, .. } = self;
        let mut injections = injections.superset();
        loop {
            match g.resume(injections) {
                EffectfulState::Perform(effects, new_g) => match effects.subset() {
                    Ok(effects) => match h(effects) {
                        ControlFlow::Continue(new_injections) => {
                            g = new_g;
                            injections = new_injections.superset();
                        },
                        ControlFlow::Break(output) => break EffectfulState::Return(output),
                    },
                    Err(effects) => {
                        break EffectfulState::Perform(
                            effects,
                            Self {
                                g: new_g,
                                h,
                                marker: PhantomData,
                            },
                        )
                    },
                },
                EffectfulState::Return(output) => break EffectfulState::Return(output),
            }
        }
    }
}

pub fn handle_group<
    G,
    H,
    HandledEs,
    PostEs,
    EffectIndices,
    InjectionIndices,
    SupersetIndices,
>(
    g: G,
    h: H,
) -> HandleGroup<
    G,
    H,
    HandledEs,
    PostEs,
    EffectIndices,
    InjectionIndices,
    SupersetIndices,
> {
    HandleGroup {
        g,
        h,
        marker: PhantomData,
    }
}

/*
/// Handle the last effect in a computation using an async handler.
///
/// For handling multiple effects asynchronously, see [`handle_group_async`]. For details on
/// handling effects, see [`handle`].
///
/// When an async handler is used, it must handle all of the remaining effects in a computation,
/// because it is impossible to construct a computation that is both asynchronous and effectful.
///
/// For more flexible interactions with Futures, see [`effects::future`].
pub async fn handle_async<G, E, Fut>(mut g: G, mut handler: impl FnMut(E) -> Fut) -> G::Output
where
    G: Effectful<Effects = Union!(E)>,
    E: Effect,
    Fut: Future<Output = ControlFlow<G::Output, E::Injection>>,
{
    let mut injections = Union::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pin = unsafe { Pin::new_unchecked(&mut g) };
        match pin.resume(injections) {
            EffectfulState::Perform(effects) => {
                let effect = match effects {
                    Union::Inl(effect) => effect,
                    Union::Inr(never) => match never {},
                };
                match handler(effect).await {
                    ControlFlow::Continue(new_injections) => {
                        injections = Union::Inl(Tagged::new(new_injections))
                    },
                    ControlFlow::Break(output) => return output,
                }
            },
            EffectfulState::Return(output) => return output,
        }
    }
}

/// Handle all of the remaining effects in a computation using an async handler.
///
/// For handling one effect asynchronously, see [`handle_async`]. For details on handling effects in
/// groups, see [`handle_group`].
///
/// When an async handler is used, it must handle all of the remaining effects in a computation,
/// because it is impossible to construct a computation that is both asynchronous and effectful.
///
/// For more flexible interactions with Futures, see [`effects::future`].
pub async fn handle_group_async<G, Es, Fut, BeginIndex>(
    mut g: G,
    mut handler: impl FnMut(Es) -> Fut,
) -> G::Output
where
    G: Effectful<Effects = Es>,
    Es: EffectList,
    Es::Injections: Inject<Begin, BeginIndex>,
    Fut: Future<Output = ControlFlow<G::Output, Es::Injections>>,
{
    let mut injections = Es::Injections::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pin = unsafe { Pin::new_unchecked(&mut g) };
        match pin.resume(injections) {
            EffectfulState::Perform(effects) => match handler(effects).await {
                ControlFlow::Continue(new_injections) => injections = new_injections,
                ControlFlow::Break(output) => return output,
            },
            EffectfulState::Return(output) => return output,
        }
    }
}

/// Handle one effect in the computation `g` by running other effects.
///
/// It is not possible to get rustc to infer the type of `PostEs`, so calling this function almost
/// always requires annotating that - which means you also have to annotate 21 underscores.
/// For this reason, prefer to use [`transform0`] or [`transform1`] instead, which should not
/// require annotation.
pub fn transform<
    R,
    H,
    E,
    PreEs,
    PreHandleEs,
    HandlerEs,
    PostEs,
    EffectIndex,
    BeginIndex1,
    BeginIndex2,
    BeginIndex3,
    InjectionIndex,
    SubsetIndices1,
    SubsetIndices2,
    SupersetIndices1,
    SupersetIndices2,
    SupersetIndices3,
>(
    mut g: impl Effectful<Effects = PreEs, Output = R>,
    mut handler: impl FnMut(E) -> H,
) -> impl Effectful<Effects = PostEs, Output = R>
where
    H: Effectful<Effects = HandlerEs, Output = E::Injection>,
    E: Effect,
    PreEs: EffectList + Uninject<E, EffectIndex, Remainder = PreHandleEs>,
    PreHandleEs: EffectList + Superset<PostEs, SupersetIndices1>,
    HandlerEs: EffectList + Superset<PostEs, SupersetIndices2>,
    PostEs: EffectList,
    PreEs::Injections: Inject<Begin, BeginIndex1>
        + Uninject<Tagged<E::Injection, E>, InjectionIndex, Remainder = PreHandleEs::Injections>,
    PreHandleEs::Injections: Superset<PreEs::Injections, SupersetIndices3>,
    HandlerEs::Injections: Inject<Begin, BeginIndex2>,
    PostEs::Injections: Inject<Begin, BeginIndex3>
        + Subset<
            <PreEs::Injections as Uninject<Tagged<E::Injection, E>, InjectionIndex>>::Remainder,
            SubsetIndices1,
        > + Subset<HandlerEs::Injections, SubsetIndices2>,
{
    GeneratorToEffectful::new(move |_begin: PostEs::Injections| {
        let mut injections = PreEs::Injections::inject(Begin);
        loop {
            // safety: see handle_group()
            let pin = unsafe { Pin::new_unchecked(&mut g) };
            match pin.resume(injections) {
                EffectfulState::Perform(effects) => match effects.uninject() {
                    // the effect we are handling
                    Ok(effect) => {
                        let mut handling = handler(effect);
                        let mut handler_injections = HandlerEs::Injections::inject(Begin);
                        'handling: loop {
                            // safety: same again
                            let pin = unsafe { Pin::new_unchecked(&mut handling) };
                            match pin.resume(handler_injections) {
                                EffectfulState::Perform(effects) => {
                                    handler_injections =
                                        PostEs::Injections::subset(yield effects.superset())
                                            .ok()
                                            .unwrap();
                                },
                                EffectfulState::Return(inj) => {
                                    injections = PreEs::Injections::inject(Tagged::new(inj));
                                    break 'handling;
                                },
                            }
                        }
                    },
                    // any other effect
                    Err(effects) => {
                        injections = PreHandleEs::Injections::superset(
                            PostEs::Injections::subset(yield effects.superset())
                                .ok()
                                .unwrap(),
                        );
                    },
                },
                EffectfulState::Return(output) => return output,
            }
        }
    })
}

/// Handle one effect of `g` by running other effects that it already uses.
///
/// This function is a special case of [`transform`] for when the handler does not introduce any
/// effects on top of the ones from `g` that it's not handling.
///
/// For introducing a new effect, see [`transform1`].
pub fn transform0<
    R,
    H,
    E,
    PreEs,
    HandlerEs,
    PostEs,
    EffectIndex,
    BeginIndex1,
    BeginIndex2,
    BeginIndex3,
    InjectionIndex,
    SubsetIndices1,
    SubsetIndices2,
    SupersetIndices1,
    SupersetIndices2,
    SupersetIndices3,
>(
    g: impl Effectful<Effects = PreEs, Output = R>,
    handler: impl FnMut(E) -> H,
) -> impl Effectful<Effects = PostEs, Output = R>
where
    H: Effectful<Effects = HandlerEs, Output = E::Injection>,
    E: Effect,
    PreEs: EffectList + Uninject<E, EffectIndex, Remainder = PostEs>,
    HandlerEs: EffectList + Superset<PostEs, SupersetIndices1>,
    PostEs: EffectList + Superset<PostEs, SupersetIndices2>,
    PreEs::Injections: Inject<Begin, BeginIndex1>
        + Uninject<Tagged<E::Injection, E>, InjectionIndex, Remainder = PostEs::Injections>,
    HandlerEs::Injections: Inject<Begin, BeginIndex2>,
    PostEs::Injections: Inject<Begin, BeginIndex3>
        + Subset<HandlerEs::Injections, SubsetIndices1>
        + Subset<PostEs::Injections, SubsetIndices2>
        + Superset<PreEs::Injections, SupersetIndices3>,
{
    transform(g, handler)
}

/// Handle one effect of `g` by running a new effect.
///
/// This function is a special case of [`transform`] for when the handler introduces one effect on
/// top of the ones from `g` that it's not handling.
///
/// It is possible for the handler to run effects from `g` as well as the effect that it introduces.
///
/// To transform without introducing any effects, see [`transform0`].
pub fn transform1<
    R,
    H,
    E1,
    E2,
    PreEs,
    PreHandleEs,
    HandlerEs,
    E1Index,
    BeginIndex1,
    BeginIndex2,
    BeginIndex3,
    I1Index,
    SubsetIndices1,
    SubsetIndices2,
    SupersetIndices1,
    SupersetIndices2,
    SupersetIndices3,
>(
    g: impl Effectful<Effects = PreEs, Output = R>,
    handler: impl FnMut(E1) -> H,
) -> impl Effectful<Effects = Union<E2, PreHandleEs>, Output = R>
where
    H: Effectful<Effects = HandlerEs, Output = E1::Injection>,
    E1: Effect,
    E2: Effect,
    PreEs: EffectList + Uninject<E1, E1Index, Remainder = PreHandleEs>,
    PreHandleEs: EffectList + Superset<Union<E2, PreHandleEs>, SupersetIndices1>,
    HandlerEs: EffectList + Superset<Union<E2, PreHandleEs>, SupersetIndices2>,
    PreEs::Injections: Inject<Begin, BeginIndex1>
        + Uninject<Tagged<E1::Injection, E1>, I1Index, Remainder = PreHandleEs::Injections>,
    PreHandleEs::Injections: Superset<PreEs::Injections, SupersetIndices3>,
    HandlerEs::Injections: Inject<Begin, BeginIndex2>,
    Union<Tagged<E2::Injection, E2>, PreHandleEs::Injections>: Inject<Begin, BeginIndex3>
        + Subset<HandlerEs::Injections, SubsetIndices1>
        + Subset<PreHandleEs::Injections, SubsetIndices2>,
{
    transform(g, handler)
}
*/
