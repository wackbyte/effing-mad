//! This program prints out the following:
//! log: I know your password! It's monadtransformerssuck
//! log: I'm going to do evil things and you can't stop me!
//! log: starting...
//! log: something went wrong! aah!
//!
//! This is done by the function `combined()`, which uses effects Cancel, Log, and FileRead, whose
//! behaviours are all specified in the effect handlers in `main()`.
//! Note that the effect definitions are written out manually in this example. For usage of the
//! effects! macro, which allows more ergonomic effect definitions, see effects_macro.rs

#![feature(generators)]
#![feature(generator_trait)]
#![feature(never_type)]

use {
    effing_mad::{effectful, handle_group, handler, lift, perform, Effect},
    std::ops::ControlFlow,
};

fn main() {
    let handled = handle_group(
        combined(),
        handler! {
            Cancel: Cancel => return ControlFlow::Break(()),
            Log(msg): Log => println!("log: {msg}"),
            FileRead(name): FileRead => {
                assert_eq!(name, "~/my passwords.txt");
                "monadtransformerssuck".into()
            },
        },
    );

    effing_mad::run(handled);
}

struct Cancel;

impl Effect for Cancel {
    type Injection = !;
}

/// The logging handler does not provide any information back to the effectful function.
struct Log<'a>(std::borrow::Cow<'a, str>);

impl<'a> Effect for Log<'a> {
    type Injection = ();
}

/// For this example, we pretend files are just strings, and the whole file is read at once.
struct FileRead(String);

impl Effect for FileRead {
    type Injection = String;
}

// This function demonstrates combining effects to represent both control flow and I/O, where the
// behaviour of the I/O is specified outside the function. In this case the behaviour comes from
// the closure passed to `handle()` in `main`.
// Allow unreachable code because the compiler knows that after cancelling, there is no more
// execution, so this function would otherwise cause a warning. However, this warning only comes up
// if there is a `yield` after cancelling, not if there are only normal statements and expressions.
#[effectful(Cancel, Log<'a>)]
fn simple<'a>() {
    perform!(Log("starting...".into()));
    perform!(Log("something went wrong! aah!".into()));
    perform!(Cancel);
    #[allow(unreachable_code)]
    {
        perform!(Log("no, sorry. i have gone home.".into()));
    }
}

// This function demonstrates how effect handlers can pass values back into the effectful function,
// and how the `do_` operator can be used to call effectful functions, as long as the callee has a
// subset of the caller's effects.
#[effectful(Cancel, Log<'a>, FileRead)]
fn combined<'a>() {
    let mischief = perform!(FileRead("~/my passwords.txt".into()));
    // this is why Log has to use Cow - we can't yield something referencing local content...
    // ...for some reason. I sure hope that doesn't foil my plans to take over Rust with algebraic
    // effects.
    perform!(Log(format!("I know your password! It's {mischief}").into()));
    perform!(Log(
        "I'm going to do evil things and you can't stop me!".into()
    ));
    lift!(simple());
}
