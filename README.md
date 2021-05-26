# ppx-remove-deriving

This PPX rewriter attempts to remove all [@@deriving] derivers unknown to ReScript. This allows you to use any Ppxlib-compatible derivers such as ppx_let, ppx_compare, ppx_sexp_conv, etc.

## Installation

For easier composition of PPXes, [`ppx-install`](https://github.com/diaozheng999/ppx-install) is recommended.

After installing `ppx-install`, add the following to `ppx` field in `package.json`, ensuring that it's the last rewriter in the list:

```js
{
  "ppx": [
    // other Ppxlib-based rewriters
    "@opam/ppx_let",
    "@opam/ppx_sexp_conv",
    "@opam/ppx_here",
    // ...
 
    // add this and ensure this is the last one
    "@nasi/ppx-remove-deriving"
  ]
}
```

## What it does

Removes the all [@@deriving] attributes **except** for:
1. `jsConverter`
2. `{ jsConverter = newType }`
3. `accessors`
4. `abstract`

## Why this is needed

Ppxlib-compatible derivers do not remove the [@@deriving] attribute after the deriver completes, as it does not perform any code rewrite, so:

```ocaml
type t [@@deriving sexp]
```

becomes

```ocaml
type t [@@deriving sexp]

(* generated by ppx_sexp_conv *)
val sexp_of_t : t -> sexp
val t_of_sexp : sexp -> t
```

This is acceptable by the ocaml bytecode and native compilers. However, in ReScript, this would throw an error, as the ReScript compiler defines its own [@@deriving] (in this case [@@bs.deriving]) attributes, and does not know what to do with the other ones. In this case, you get the following error:

```
FAILED: src/utils/json.iast

  We've found a bug for you!
  src/utils/json.resi:1:11-14

  1 │ @deriving(sexp)
  2 │ type t = Js.Json.t

  sexp is not supported
```

To overcome this error, this deriver simply remove all `deriving` attributes that's unrelated to ReScript. In this way,

```rescript
@deriving(sexp)
type t
@deriving(sexp, accessors)
type u = {
  // whatever, something that allows accessors to be generated
}
```

will be written as

```rescript
type t // notice the lack of @deriving annotation

// generated by ppx_sexp_conv
let sexp_of_t : t => sexp
let t_of_sexp : sexp => t

@deriving(accessors) // incompatible deriver sexp is removed
type u = {
  // ...
}

// generated by ppx_sexp_conv
let sexp_of_u : u => sexp
let u_of_sexp : sexp => u
```

