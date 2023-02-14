//! Types and functions that allow writing nondeterministic code.

use {
    crate::{
        data::Union,
        injection::{Begin, Tagged},
        Effect,
    },
    alloc::vec::Vec,
    core::{
        ops::{Generator, GeneratorState},
        pin::pin,
    },
};

/// Nondeterminism effect.
///
/// This is analogous to Haskell's list monad. It allows writing sequential-looking functions that
/// might split into multiple "branches", causing execution of the remainder of the function to
/// occur multiple times with each value from the effect being given to one branch.
#[derive(Clone)]
pub struct Nondet<T>(pub Vec<T>);

impl<T> Effect for Nondet<T> {
    type Injection = T;
}

/// Run a nondeterministic computation, collecting all the resulting return values into a `Vec`.
pub fn run_nondet<G, T>(g: G) -> Vec<G::Return>
where
    G: Generator<Union!(Tagged<T, Nondet<T>>, Begin), Yield = Union!(Nondet<T>)> + Clone,
{
    let mut outputs = Vec::new();
    run_nondet_inner(g, Union::inject(Begin), &mut outputs);
    outputs
}

fn run_nondet_inner<G, T>(
    mut g: G,
    injections: Union!(Tagged<T, Nondet<T>>, Begin),
    outputs: &mut Vec<G::Return>,
) where
    G: Generator<Union!(Tagged<T, Nondet<T>>, Begin), Yield = Union!(Nondet<T>)> + Clone,
{
    let mut pin = pin!(g);
    match pin.as_mut().resume(injections) {
        GeneratorState::Yielded(effects) => {
            let Nondet(xs) = match effects {
                Union::Inl(nondet) => nondet,
                Union::Inr(never) => match never {},
            };
            for x in xs {
                let g2 = pin.clone();
                run_nondet_inner(g2, Union::inject(Tagged::new(x)), outputs);
            }
        },
        GeneratorState::Complete(output) => outputs.push(output),
    }
}
