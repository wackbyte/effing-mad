//! Standard effects and functions to make them useful.

pub use future::{EffectfulExt, FutureExt};
#[cfg(feature = "nondet")]
pub use nondet::{run_nondet, Nondet};

pub mod future;
#[cfg(feature = "nondet")]
pub mod nondet;
pub mod state;
