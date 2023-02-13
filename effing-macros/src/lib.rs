use {
    proc_macro::TokenStream,
    quote::{quote, ToTokens},
    syn::{
        parse::{Parse, ParseStream},
        parse_macro_input, parse_quote,
        punctuated::Punctuated,
        Error, Expr, ItemFn, Pat, ReturnType, Signature, Token, Type,
    },
};

struct Effectful {
    effects: Punctuated<Type, Token![,]>,
}

impl Parse for Effectful {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let effects = Punctuated::parse_terminated(input)?;
        Ok(Effectful { effects })
    }
}

/// Define an effectful function using `fn`-like syntax.
///
/// Effectful functions can suspend their execution, and when called immediately return an
/// effectful computation. This is analogous to `async fn`s, which can also suspend their
/// execution, and return a `Future` when called.
///
/// # Usage
/// ```rust
/// #[effectful(A, B)]
/// /* optionally: */ #[effectful::cloneable]
/// fn cool_function(arg: Foo) -> Bar {
///     yield expr_a;
///     let val = yield expr_b;
///     epic_function(val).do_
/// }
/// ```
/// This macro takes a list of types as its arguments. These types must implement `Effect` or
/// `EffectGroup`. Expressions passed to `yield` must be of one of those effect types, or of an
/// effect type that is in one of those groups.
///
/// The `yield expr` syntax runs the effect `expr`, and evaluates to the Injection of that effect
/// type.
///
/// The `do_` operator is analogous to `.await`. It runs an effectful computation by yielding all
/// of its effects to the caller of the current function. The callee's effects must be a subset of
/// the current function's effects - in this example, a subset of `{A, B}`. The callee is usually
/// another function defined using this macro.
///
/// It is possible to create effectful functions whose computations can be cloned. This requires
/// marking the function as `#[effectful::cloneable]` after the `#[effectful(...)]` invocation,
/// the function to have only `Clone` and `Unpin` locals, and the function to never hold a
/// reference to a local across a yield point. In other words, the underlying generator must be
/// `Clone` and `Unpin`.
#[proc_macro_attribute]
pub fn effectful(args: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(args as Effectful);
    let effect_names = input.effects.iter();
    let yield_type = quote! {
        <::effing_mad::frunk::Coprod!(#(#effect_names),*) as ::effing_mad::macro_impl::FlattenEffects>::Out
    };
    let ItemFn {
        mut attrs,
        vis,
        sig,
        block,
    } = parse_macro_input!(item as ItemFn);
    let Signature {
        constness,
        unsafety,
        ident,
        generics,
        inputs,
        output,
        ..
    } = sig;
    let return_type = match output {
        ReturnType::Default => quote!(()),
        ReturnType::Type(_r_arrow, ref ty) => ty.to_token_stream(),
    };
    let mut cloneable = false;
    attrs.retain(|attr| {
        if attr.path == parse_quote!(effectful::cloneable) {
            cloneable = true;
            false // remove it from the attrs list so no one gets confused
        } else {
            true
        }
    });
    let clone_bound = cloneable.then_some(quote!( + ::core::clone::Clone + ::core::marker::Unpin));
    quote! {
        #(#attrs)*
        #vis #constness #unsafety
        fn #ident #generics(#inputs)
        -> impl ::core::ops::Generator<
            <#yield_type as ::effing_mad::injection::EffectList>::Injections,
            Yield = #yield_type,
            Return = #return_type
        > #clone_bound {
            move |_begin: <#yield_type as ::effing_mad::injection::EffectList>::Injections| {
                #block
            }
        }
    }
    .into()
}

struct HandlerArm {
    // TODO: attributes and guards?
    pat: Pat,
    colon_token: Token![:],
    ty: Type,
    fat_arrow_token: Token![=>],
    body: Expr,
    comma: Option<Token![,]>,
}

impl Parse for HandlerArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pat: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
            fat_arrow_token: input.parse()?,
            body: input.parse()?,
            comma: input.parse()?,
        })
    }
}

struct Handler {
    asyncness: Option<Token![async]>,
    moveness: Option<Token![move]>,
    arms: Vec<HandlerArm>,
}

impl Parse for Handler {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let asyncness = input.parse()?;
        let moveness = input.parse()?;

        let mut arms = Vec::new();
        while !input.is_empty() {
            arms.push(input.parse()?);
        }

        Ok(Handler {
            asyncness,
            moveness,
            arms,
        })
    }
}

/// Define a handler for an effect or group of effects.
///
/// # Usage
/// Handling a group of effects at once:
/// ```rust
/// let mut state = 0i32;
/// handle_group(
///     g,
///     handler! {
///         State<i32> {
///             get() => state,
///             put(v) => state = v,
///         }
///     }
/// )
/// ```
///
/// Handling a single effect at once:
/// ```
/// handle(g, handler!(Cancel => break))
/// ```
///
/// The value that a handler arm's expression evaluates to (for example `state` and `()` in the
/// `State<i32>` example) is used as the injection for that effect. It is also possible to use the
/// `break` keyword to cause the computation that is being handled to return. In this case, the
/// type of the value passed to `break` must match the return type of a computation that the
/// handler is used on. See `effing_mad::map` for a way to change the return value of a computation.
///
/// The handler can capture state from its environment, and/or be asynchronous. The keywords
/// `async` and `move` can both optionally appear (in that order) at the very beginning of the
/// macro input to control these behaviours, in a similar way to how they would affect a closure.
/// These keywords apply to all arms of the handler. `handle_async` or `handle_group_async` must be
/// used when applying an async handler.
///
/// Note that the `put` arm in this example mutably borrows the `state` variable, while the `get`
/// arm also borrows it. This is the advantage of handling effects together. Internally, `handler!`
/// expands to a single closure with a `match` expression in it, so the arms can all borrow the
/// same content, even mutably.
#[proc_macro]
pub fn handler(input: TokenStream) -> TokenStream {
    let Handler {
        asyncness,
        moveness,
        arms,
    } = parse_macro_input!(input as Handler);

    let mut matcher = quote! { match effs {} };
    let mut eff_tys = Vec::new();
    for arm in arms.into_iter().rev() {
        // TODO: attrs, guard?
        let HandlerArm { pat, ty, body, .. } = arm;

        matcher = quote! {
            match ::effing_mad::frunk::coproduct::CoprodUninjector::uninject(effs) {
                Ok(#pat) => {
                    let __effing_inj = #body;
                    #[allow(unreachable_code)]
                    ::effing_mad::frunk::coproduct::CoprodInjector::inject(
                        ::effing_mad::injection::Tagged::<_, #ty>::new(
                            __effing_inj
                        )
                    )
                },
                Err(effs) => #matcher,
            }
        };
        eff_tys.push(ty);
    }
    let effs_ty = quote!(::effing_mad::frunk::Coprod!(#(#eff_tys),*));

    quote! {
        #moveness |effs: #effs_ty| #asyncness {
            let __effing_inj = #matcher;
            // if the handler unconditionally breaks then this line is unreachable, but we
            // don't want to see a warning for it.
            #[allow(unreachable_code)]
            ::core::ops::ControlFlow::<_, _>::Continue(__effing_inj)
        }
    }
    .into()
}
