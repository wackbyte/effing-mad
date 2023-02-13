#![feature(generator_trait, generators)]

use effing_mad::{effectful, handle, handler, lift, perform, run, Effect};

struct Cancel;

impl Effect for Cancel {
    type Injection = effing_mad::Never;
}

#[effectful(Cancel)]
fn cancel() {
    perform!(Cancel);
}

fn main() {
    let handled = handle(cancel(), handler! { Cancel => break });
    run(handled);
}
