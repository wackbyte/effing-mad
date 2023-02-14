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
pub mod higher_order;
pub mod injection;

#[doc(hidden)]
pub mod macro_impl;

pub use self::macro_impl::{lift, perform};
pub use effing_macros::{effectful, handler};

use {
    self::{
        data::{union, Union, Void},
        injection::{Begin, EffectList, Tagged},
    },
    core::{
        future::Future,
        ops::{ControlFlow, Generator, GeneratorState},
        pin::{pin, Pin},
    },
};

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
    type Effects = Union<E, Void>;
}

/// Run an effectful computation that has no effects.
///
/// Effectful computations are generators, but if they have no effects, it is guaranteed that they
/// will never yield. Therefore they can be run by resuming them once. This function does that.
pub fn run<R>(mut g: impl Generator<Union<Begin, Void>, Yield = Void, Return = R>) -> R {
    let pin = pin!(g);
    match pin.resume(Union::Inl(Begin)) {
        GeneratorState::Yielded(never) => match never {},
        GeneratorState::Complete(output) => output,
    }
}

/// Create a new effectful computation by applying a "pure" function to the return value of an
/// existing computation.
pub fn map<E, I, T, U>(
    mut g: impl Generator<I, Yield = E, Return = T>,
    f: impl FnOnce(T) -> U,
) -> impl Generator<I, Yield = E, Return = U> {
    move |mut injection: I| {
        loop {
            // safety: see handle_group()
            let pinned = unsafe { Pin::new_unchecked(&mut g) };
            match pinned.resume(injection) {
                GeneratorState::Yielded(effect) => injection = yield effect,
                GeneratorState::Complete(output) => return f(output),
            }
        }
    }
}

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
    PreIs,
    PostIs,
    EffectIndex,
    BeginIndex,
    InjectionIndex,
    InjectionIndices,
    SupersetIndices,
>(
    g: impl Generator<PreIs, Yield = PreEs, Return = R>,
    mut handler: impl FnMut(E) -> ControlFlow<R, E::Injection>,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    E: Effect,
    PreEs: EffectList<Injections = PreIs> + union::Uninject<E, EffectIndex, Remainder = PostEs>,
    PostEs: EffectList<Injections = PostIs>,
    Union!(Tagged<E::Injection, E>, Begin): union::Superset<PreIs, InjectionIndices>,
    PreIs:
        union::Inject<Begin, BeginIndex> + union::Inject<Tagged<E::Injection, E>, InjectionIndex>,
    PostIs: union::Superset<PreIs, SupersetIndices>,
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
    Is,
    PreIs,
    PostIs,
    EffectIndices,
    BeginIndex,
    InjectionIndices,
    SupersetIndices,
>(
    mut g: impl Generator<PreIs, Yield = PreEs, Return = R>,
    mut handler: impl FnMut(Es) -> ControlFlow<R, Is>,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    Es: EffectList<Injections = Is>,
    PreEs: EffectList<Injections = PreIs> + union::Subset<Es, EffectIndices, Remainder = PostEs>,
    PostEs: EffectList<Injections = PostIs>,
    Is: union::Superset<PreIs, InjectionIndices>,
    PreIs: union::Inject<Begin, BeginIndex>,
    PostIs: union::Superset<PreIs, SupersetIndices>,
{
    move |_begin: PostIs| {
        let mut injections = PreIs::inject(Begin);
        loop {
            // safety: im 90% sure that since we are inside Generator::resume which takes
            // Pin<&mut self>, all locals in this function are effectively pinned and this call is
            // simply projecting that
            let pin = unsafe { Pin::new_unchecked(&mut g) };
            match pin.resume(injections) {
                GeneratorState::Yielded(effects) => match effects.subset() {
                    Ok(effects) => match handler(effects) {
                        ControlFlow::Continue(new_injections) => {
                            injections = new_injections.superset()
                        },
                        ControlFlow::Break(ret) => return ret,
                    },
                    Err(effects) => injections = (yield effects).superset(),
                },
                GeneratorState::Complete(ret) => return ret,
            }
        }
    }
}

/// Handle the last effect in a computation using an async handler.
///
/// For handling multiple effects asynchronously, see [`handle_group_async`]. For details on
/// handling effects, see [`handle`].
///
/// When an async handler is used, it must handle all of the remaining effects in a computation,
/// because it is impossible to construct a computation that is both asynchronous and effectful.
///
/// For more flexible interactions with Futures, see [`effects::future`].
pub async fn handle_async<G, E, Fut>(mut g: G, mut handler: impl FnMut(E) -> Fut) -> G::Return
where
    G: Generator<Union!(Tagged<E::Injection, E>, Begin), Yield = Union!(E)>,
    E: Effect,
    Fut: Future<Output = ControlFlow<G::Return, E::Injection>>,
{
    let mut injections = Union::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pin = unsafe { Pin::new_unchecked(&mut g) };
        match pin.resume(injections) {
            GeneratorState::Yielded(effects) => {
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
            GeneratorState::Complete(output) => return output,
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
pub async fn handle_group_async<G, Es, Is, Fut, BeginIndex>(
    mut g: G,
    mut handler: impl FnMut(Es) -> Fut,
) -> G::Return
where
    G: Generator<Is, Yield = Es>,
    Es: EffectList<Injections = Is>,
    Is: union::Inject<Begin, BeginIndex>,
    Fut: Future<Output = ControlFlow<G::Return, Is>>,
{
    let mut injections = Is::inject(Begin);
    loop {
        // safety: see handle_group() - remember that futures are pinned in the same way as
        // generators
        let pin = unsafe { Pin::new_unchecked(&mut g) };
        match pin.resume(injections) {
            GeneratorState::Yielded(effects) => match handler(effects).await {
                ControlFlow::Continue(new_injections) => injections = new_injections,
                ControlFlow::Break(output) => return output,
            },
            GeneratorState::Complete(output) => return output,
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
    PreIs,
    PreHandleIs,
    HandlerIs,
    PostIs,
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
    mut g: impl Generator<PreIs, Yield = PreEs, Return = R>,
    mut handler: impl FnMut(E) -> H,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    H: Generator<HandlerIs, Yield = HandlerEs, Return = E::Injection>,
    E: Effect,
    PreEs:
        EffectList<Injections = PreIs> + union::Uninject<E, EffectIndex, Remainder = PreHandleEs>,
    PreHandleEs: EffectList<Injections = PreHandleIs> + union::Superset<PostEs, SupersetIndices1>,
    HandlerEs: EffectList<Injections = HandlerIs> + union::Superset<PostEs, SupersetIndices2>,
    PostEs: EffectList<Injections = PostIs>,
    PreIs: union::Inject<Begin, BeginIndex1>
        + union::Uninject<Tagged<E::Injection, E>, InjectionIndex, Remainder = PreHandleIs>,
    PreHandleIs: union::Superset<PreIs, SupersetIndices3>,
    HandlerIs: union::Inject<Begin, BeginIndex2>,
    PostIs: union::Inject<Begin, BeginIndex3>
        + union::Subset<
            <PreIs as union::Uninject<Tagged<E::Injection, E>, InjectionIndex>>::Remainder,
            SubsetIndices1,
        > + union::Subset<HandlerIs, SubsetIndices2>,
{
    move |_begin: PostIs| {
        let mut injections = PreIs::inject(Begin);
        loop {
            // safety: see handle_group()
            let pin = unsafe { Pin::new_unchecked(&mut g) };
            match pin.resume(injections) {
                GeneratorState::Yielded(effects) => match effects.uninject() {
                    // the effect we are handling
                    Ok(effect) => {
                        let mut handling = handler(effect);
                        let mut handler_injections = HandlerIs::inject(Begin);
                        'handling: loop {
                            // safety: same again
                            let pin = unsafe { Pin::new_unchecked(&mut handling) };
                            match pin.resume(handler_injections) {
                                GeneratorState::Yielded(effects) => {
                                    handler_injections =
                                        PostIs::subset(yield effects.superset()).ok().unwrap();
                                },
                                GeneratorState::Complete(inj) => {
                                    injections = PreIs::inject(Tagged::new(inj));
                                    break 'handling;
                                },
                            }
                        }
                    },
                    // any other effect
                    Err(effects) => {
                        injections = PreHandleIs::superset(
                            PostIs::subset(yield effects.superset()).ok().unwrap(),
                        );
                    },
                },
                GeneratorState::Complete(output) => return output,
            }
        }
    }
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
    PreIs,
    HandlerIs,
    PostIs,
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
    g: impl Generator<PreIs, Yield = PreEs, Return = R>,
    handler: impl FnMut(E) -> H,
) -> impl Generator<PostIs, Yield = PostEs, Return = R>
where
    H: Generator<HandlerIs, Yield = HandlerEs, Return = E::Injection>,
    E: Effect,
    PreEs: EffectList<Injections = PreIs> + union::Uninject<E, EffectIndex, Remainder = PostEs>,
    HandlerEs: EffectList<Injections = HandlerIs> + union::Superset<PostEs, SupersetIndices1>,
    PostEs: EffectList<Injections = PostIs> + union::Superset<PostEs, SupersetIndices2>,
    PreIs: union::Inject<Begin, BeginIndex1>
        + union::Uninject<Tagged<E::Injection, E>, InjectionIndex, Remainder = PostIs>,
    HandlerIs: union::Inject<Begin, BeginIndex2>,
    PostIs: union::Inject<Begin, BeginIndex3>
        + union::Subset<HandlerIs, SubsetIndices1>
        + union::Subset<PostIs, SubsetIndices2>
        + union::Superset<PreIs, SupersetIndices3>,
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
    PreIs,
    PreHandleIs,
    HandlerIs,
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
    g: impl Generator<PreIs, Yield = PreEs, Return = R>,
    handler: impl FnMut(E1) -> H,
) -> impl Generator<
    Union<Tagged<E2::Injection, E2>, PreHandleIs>,
    Yield = Union<E2, PreHandleEs>,
    Return = R,
>
where
    E1: Effect,
    E2: Effect,
    H: Generator<HandlerIs, Yield = HandlerEs, Return = E1::Injection>,
    PreEs: EffectList<Injections = PreIs> + union::Uninject<E1, E1Index, Remainder = PreHandleEs>,
    PreHandleEs: EffectList<Injections = PreHandleIs>
        + union::Superset<Union<E2, PreHandleEs>, SupersetIndices1>,
    HandlerEs: EffectList<Injections = HandlerIs>
        + union::Superset<Union<E2, PreHandleEs>, SupersetIndices2>,
    PreIs: union::Inject<Begin, BeginIndex1>
        + union::Uninject<Tagged<E1::Injection, E1>, I1Index, Remainder = PreHandleIs>,
    PreHandleIs: union::Superset<PreIs, SupersetIndices3>,
    HandlerIs: union::Inject<Begin, BeginIndex2>,
    Union<Tagged<E2::Injection, E2>, PreHandleIs>: union::Inject<Begin, BeginIndex3>
        + union::Subset<HandlerIs, SubsetIndices1>
        + union::Subset<PreHandleIs, SubsetIndices2>,
{
    transform(g, handler)
}
