#![feature(generator_trait, generators, never_type)]

use {
    effing_mad::{
        data::Union, effectful, handle_group, handler, perform, run, Effect, Effectful,
        GeneratorToEffectful,
    },
    std::ops::ControlFlow,
};

struct Number(usize);

impl Effect for Number {
    type Injection = ();
}

fn generator_fn() -> impl Effectful<Effects = Union!(Number), Output = ()> {
    GeneratorToEffectful::new(Box::pin(static |_begin| {
        let foo = String::from("foo");
        let foo_ref = &foo;
        perform!(Number(0));
        println!("{foo_ref}");
        perform!(Number(foo.len()));
    }))
}

#[effectful(Number)]
fn sussy() {
    let foo = String::from("foo");
    let foo_ref = &foo;
    perform!(Number(0));
    println!("{foo_ref}");
    perform!(Number(foo.len()));
}

fn main() {
    let handled = handle_group(
        generator_fn(),
        handler! {
            Number(_): Number => (),
        },
    );
    run(handled);
}
