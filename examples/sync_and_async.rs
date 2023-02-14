#![feature(generators)]
#![feature(generator_trait)]

use effing_mad::{
    data::Union, effectful, handle_group, handle_group_async, handler, perform, run, Effect,
    EffectGroup,
};

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(interesting_and_useful());

    boring_and_old_fashioned();
}

struct HttpGet(&'static str);

impl Effect for HttpGet {
    type Injection = String;
}

struct HttpRequest;

impl EffectGroup for HttpRequest {
    type Effects = Union!(HttpGet);
}

// this function does not specify whether the request happens synchronously or asynchronously
#[effectful(HttpGet)]
fn example() -> usize {
    let body = perform!(HttpGet("http://example.com"));
    body.len()
}

async fn interesting_and_useful() {
    let handler = handler! { async
        HttpGet(url): HttpGet => reqwest::get(url).await.unwrap().text().await.unwrap(),
    };

    let req1 = handle_group_async(example(), handler);
    let req2 = handle_group_async(example(), handler);

    // asyncified effectful functions can be composed in the same ways as traditional futures
    let (res1, res2) = futures::future::join(req1, req2).await;
    println!("asynchronously found {res1} and {res2} bytes");
}

fn boring_and_old_fashioned() {
    let handler = handler! {
        HttpGet(url): HttpGet => reqwest::blocking::get(url).unwrap().text().unwrap(),
    };

    let req = handle_group(example(), handler);
    let res = run(req);
    println!("synchronously found {res} bytes");
}
