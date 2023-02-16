//! Implementation details of the macros exported by `effing_mad`.

use {
    crate::{
        data::{union::Uninject, Union, Void},
        injection::{EffectList, Tagged},
        Effect, EffectGroup,
    },
    core::marker::PhantomData,
};

pub macro perform($effect:expr $(,)?) {{
    let effect = $crate::IntoEffect::into_effect($effect);
    let marker = $crate::macro_impl::marker(&effect);
    let injections = yield $crate::data::Union::inject(effect);
    $crate::macro_impl::uninject_with_marker(injections, marker).unwrap()
}}

pub macro lift($effectful:expr $(,)?) {{
    let mut effectful = $crate::IntoEffectful::into_effectful($effectful);
    let mut injections = $crate::data::Union::inject($crate::injection::Begin);
    loop {
        match $crate::Effectful::resume(effectful, injections) {
            $crate::EffectfulState::Perform(effects, new_effectful) => {
                effectful = new_effectful;
                injections = $crate::data::union::Subset::subset(
                    yield $crate::data::union::Superset::superset(effects),
                )
                .ok()
                .unwrap();
            },
            $crate::EffectfulState::Return(output) => break output,
        }
    }
}}

/// Construct a `PhantomData` with a type parameter determined by a value.
#[must_use]
pub fn marker<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

/// Retrieve a certain Effect's injection from a Coproduct of tagged injections.
///
/// The marker argument isn't necessary in isolation, as the type parameter E can be specified with
/// a turbofish or via inference. However, [`effing_macros::effectful`] needs a way to specify E
/// without naming any types at all. The marker argument along with [`mark`] allows specifying E
/// by naming a value instead.
pub fn uninject_with_marker<E, Is, Index>(
    injections: Is,
    _marker: PhantomData<E>,
) -> Option<E::Injection>
where
    E: Effect,
    Is: Uninject<Tagged<E::Injection, E>, Index>,
{
    injections.uninject().ok().map(Tagged::untag)
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

impl<G, Tail> FlattenEffects for Union<G, Tail>
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

impl<Tail> Prepend<Tail> for Void {
    type Out = Tail;
}

impl<Head, Tail1, Tail2> Prepend<Tail2> for Union<Head, Tail1>
where
    Tail1: Prepend<Tail2>,
{
    type Out = Union<Head, Tail1::Out>;
}
