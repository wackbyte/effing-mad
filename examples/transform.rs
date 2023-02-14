#![feature(generators)]
#![feature(generator_trait)]

use {
    effing_mad::{effectful, handle, perform, run, transform0, transform1, Effect},
    std::ops::ControlFlow,
};

fn main() {
    let work = take_over_the_world();
    // Log, Lunchtime -> Print, Lunchtime
    // Introducing 1 new effect (Print) so must use transform1
    let transformed = transform1(work, print_log);
    // Print, Lunchtime -> Print
    // Not introducing new effects so must use transform0
    let transformed = transform0(transformed, print_lunchtime);
    let handled = handle(transformed, |Print(message)| {
        println!("{message}");
        ControlFlow::Continue(())
    });
    run(handled);
}

struct Print(String);
impl Effect for Print {
    type Injection = ();
}

struct Log(String, i32);
impl Effect for Log {
    type Injection = ();
}

struct Lunchtime;
impl Effect for Lunchtime {
    type Injection = ();
}

// I am running out of inspiration for the functions in these examples
#[effectful(Log, Lunchtime)]
fn take_over_the_world() {
    perform!(Log("I intend to take over the world!".into(), 5));
    perform!(Log("I'm off to get lunch first though".into(), 0));
    perform!(Lunchtime);
    perform!(Log(
        "They're out of sausage rolls at the bakery!".into(),
        100
    ));
}

#[effectful(Print)]
fn print_log(Log(message, importance): Log) {
    perform!(Print(format!("log (importance {importance}): {message}")));
}

#[effectful(Print)]
fn print_lunchtime(Lunchtime: Lunchtime) {
    perform!(Print("lunchtime: in progress...".into()));
    std::thread::sleep(std::time::Duration::from_secs(3));
    perform!(Print("lunchtime: failed!".into()));
}
