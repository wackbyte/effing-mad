//! A demonstration of the effects! and handler! macros, which allow you to group effects into one
//! type, and use one handler for all of them. The primary benefit of this is that this handler can
//! own or mutably borrow some data, and that data can be accessed by all of the arms of the
//! handler.
//! Here we use State as a classic (and generic!) example of an effect. The same program could just
//! be written using a mutable variable, but that's no fun.

#![feature(generators)]
#![feature(generator_trait)]

use core::marker::PhantomData;
use effing_mad::{
    effectful, frunk::Coprod, handle_group, handler, perform, run, Effect, EffectGroup,
};

fn main() {
    let mut state = 34;
    let state_handler = handler! {
        Get(_): Get<_> => state,
        Put(v): Put<_> => state = v,
    };
    let handled = handle_group(use_state(), state_handler);
    run(handled);
    println!("final value: {}", state);
}

struct Get<T>(PhantomData<fn() -> T>);

impl<T> Effect for Get<T> {
    type Injection = T;
}

struct Put<T>(T);

impl<T> Effect for Put<T> {
    type Injection = ();
}

struct State<T>(PhantomData<T>);

impl<T> EffectGroup for State<T> {
    type Effects = Coprod!(Get<T>, Put<T>);
}

// Rust encourages immutability!
#[effectful(State<i32>)]
fn use_state() {
    let initial = perform!(Get(PhantomData));
    println!("initial value: {}", initial);
    perform!(Put(initial + 5));
}
