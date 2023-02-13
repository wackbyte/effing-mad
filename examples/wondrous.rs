#![feature(generator_trait, generators, never_type)]

use {
    effing_mad::{effectful, handle_group, handler, perform, run, Effect},
    std::ops::ControlFlow,
};

struct Cancel;

impl Effect for Cancel {
    type Injection = !;
}

#[effectful(Cancel)]
fn cancel() {
    perform!(Cancel);
}

fn main() {
    let handled = handle_group(
        cancel(),
        handler! {
            Cancel: Cancel => return ControlFlow::Break(()),
        },
    );
    run(handled);
}
