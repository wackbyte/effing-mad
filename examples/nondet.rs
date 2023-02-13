//!   send
//! + more
//! ------
//!  money
//!
//! solve for s, e, n, d, m, o, r, y

#![feature(generators)]
#![feature(generator_trait)]
#![feature(generator_clone)]

use effing_mad::{
    effectful,
    effects::{run_nondet, Nondet},
    lift, perform,
};

fn main() {
    println!("{:?}", run_nondet(send_more_money()));
}

// Halt this walk of the computation if pred is false
#[effectful(Nondet<u8>)]
#[effectful::cloneable]
fn guard(pred: bool) {
    if !pred {
        perform!(Nondet(Vec::new()));
    }
}

#[effectful(Nondet<u8>)]
#[effectful::cloneable]
fn send_more_money() -> [u8; 8] {
    let mut digits: Nondet<u8> = Nondet((0..=9).collect());
    let s = perform!(digits.clone());
    digits.0.retain(|&digit| digit != s);
    let e = perform!(digits.clone());
    digits.0.retain(|&digit| digit != e);
    let n = perform!(digits.clone());
    digits.0.retain(|&digit| digit != n);
    let d = perform!(digits.clone());
    digits.0.retain(|&digit| digit != d);
    let m = perform!(digits.clone());
    digits.0.retain(|&digit| digit != m);
    let o = perform!(digits.clone());
    digits.0.retain(|&digit| digit != o);
    let r = perform!(digits.clone());
    digits.0.retain(|&digit| digit != r);
    let y = perform!(digits.clone());

    lift!(guard(y == (d + e) % 10));
    let c1 = (d + e) / 10;
    lift!(guard(e == (n + r + c1) % 10));
    let c2 = (n + r + c1) / 10;
    lift!(guard(n == (e + o + c2) % 10));
    let c3 = (e + o + c2) / 10;
    lift!(guard(o == (s + m + c3) % 10));
    lift!(guard(m == (s + m + c3) / 10));

    [s, e, n, d, m, o, r, y]
}
