//! Implementation details of the macros exported by `effing_mad`.

use crate::{
    injection::{EffectList, Tagged},
    Effect, EffectGroup,
};
use core::{marker::PhantomData, ops::ControlFlow};
use frunk::{
    coproduct::{CNil, CoprodUninjector},
    Coproduct,
};

#[macro_export]
macro_rules! perform {
    ($effect:expr $(,)?) => {{
        let effect = $effect;
        let marker = ::effing_mad::macro_impl::mark(&effect);
        let injs = yield $crate::frunk::Coproduct::inject(effect);
        $crate::macro_impl::get_inj(injs, marker).unwrap()
    }};
}

#[macro_export]
macro_rules! lift {
    ($computation:expr $(,)?) => {{
        let mut gen = $computation;
        let mut injection = $crate::frunk::Coproduct::inject($crate::injection::Begin);
        loop {
            // interesting hack to trick the borrow checker
            // allows cloneable generators
            let res = {
                // safety: same as in `handle_group`
                let pinned = unsafe { ::core::pin::Pin::new_unchecked(&mut gen) };
                ::core::ops::Generator::resume(pinned, injection)
            };
            match res {
                ::core::ops::GeneratorState::Yielded(effs) => {
                    injection = $crate::frunk::coproduct::CoproductSubsetter::subset(
                        yield $crate::frunk::coproduct::CoproductEmbedder::embed(effs),
                    )
                    .ok()
                    .unwrap();
                },
                ::core::ops::GeneratorState::Complete(v) => break v,
            }
        }
    }};
}

/// Construct a `PhantomData` with a type parameter determined by a value.
#[must_use]
pub fn mark<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

/// Retrieve a certain Effect's injection from a Coproduct of tagged injections.
///
/// The marker argument isn't necessary in isolation, as the type parameter E can be specified with
/// a turbofish or via inference. However, [`effing_macros::effectful`] needs a way to specify E
/// without naming any types at all. The marker argument along with [`mark`] allows specifying E
/// by naming a value instead.
pub fn get_inj<E, Injs, Index>(injs: Injs, _marker: PhantomData<E>) -> Option<E::Injection>
where
    E: Effect,
    Injs: CoprodUninjector<Tagged<E::Injection, E>, Index>,
{
    injs.uninject().ok().map(Tagged::untag)
}

/// A type-level function from lists of `Effect`s and `EffectGroup`s to lists of `Effects` only.
///
/// This allows groups and effects to be listed together in the definition of an effectful
/// function.
pub trait FlattenEffects {
    /// The return "value" of this type-level function.
    ///
    /// Since it's a type-level function, the return "value" is a type. The return "type" is
    /// `EffectList`, which is a trait. Huh?
    type Out: EffectList;
}

impl<G, Tail> FlattenEffects for Coproduct<G, Tail>
where
    G: EffectGroup,
    <G as EffectGroup>::Effects: Prepend<Tail>,
    <<G as EffectGroup>::Effects as Prepend<Tail>>::Out: EffectList,
{
    type Out = <<G as EffectGroup>::Effects as Prepend<Tail>>::Out;
}

/// A type-level function for concatenating two Coproducts.
///
/// Arguably, this represents the disjoint union of the Coproducts. It should only be the union, but
/// I don't think that's possible. Don't concatenate overlapping coproducts, kids.
pub trait Prepend<Tail> {
    /// The return "value" of this type-level function.
    ///
    /// Since it's a type-level function, the return "value" is a type. The return "type" in this
    /// case is... nothing? There are no constraints on it. In the implementation though, the return
    /// "type" is the set of type lists represented with `Coproduct` as cons and
    /// [`CNil`](frunk::coproduct::CNil) as nil.
    type Out;
}

impl<Tail> Prepend<Tail> for CNil {
    type Out = Tail;
}

impl<Head, Tail1: Prepend<Tail2>, Tail2> Prepend<Tail2> for Coproduct<Head, Tail1> {
    type Out = Coproduct<Head, Tail1::Out>;
}
