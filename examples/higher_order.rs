//! The `effing_mad::higher_order` module contains variants of standard higher-order functions (such
//! as `Option::map`) that take effectful functions instead of "pure" ones. This allows some pretty
//! freaky control flow, like what we see here where the mapper function fails.

#![feature(generators)]
#![feature(generator_trait)]
#![feature(never_type)]

use {
    effing_mad::{
        effectful, handle_group, handler, higher_order::OptionExt, lift, perform, run, Effect,
    },
    std::ops::ControlFlow,
};

fn main() {
    let handler = handler! {
        Fail(msg): Fail => {
            println!("Error: {msg}");
            return ControlFlow::Break(());
        }
    };
    run(handle_group(precarious(), handler));
}

struct Fail(&'static str);

impl Effect for Fail {
    type Injection = !;
}

#[effectful(Fail)]
fn precarious() {
    let null_plus_five = lift!(None.map_eff(add_5));
    dbg!(null_plus_five);
    let four_plus_five = lift!(Some(4).map_eff(add_5));
    dbg!(four_plus_five);
    let forty_five_plus_five = lift!(Some(45).map_eff(add_5));
    dbg!(forty_five_plus_five); // psst - unreachable!
}

#[effectful(Fail)]
fn add_5(n: i32) -> i32 {
    if n == 45 {
        perform!(Fail("I'm scared of 45 :("));
    } else {
        n + 5
    }
}
