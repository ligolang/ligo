---
id: v1-migration-guide
title: Migration to LIGO v. 1.0
---

import Syntax from '@theme/Syntax';

# Migration to LIGO v. 1.0

Exciting news, LIGO has released version 1.0! We made sure to fit in this release a number of pending breaking changes, so that our users do not need to catch up with small breaking changes every release, and can handle the migration in bulk. Please continue reading to learn which changes may affect your existing codebase.

## All syntaxes

### No more `main` function

You should not manually craft a `main` function that calls your entry points anymore. Instead, above each entry point function, you can now write `@entry` for JsLIGO or `[@entry]` for CameLIGO. This will cause a `main` function to be automatically generated behind the scenes.

If you need more fine-grained control, it is still possible to write a `main` function, but you will need to add `@entry` or `[@entry]` above that `main` function (and only that function). See [the documentation on the main function and entry points](../advanced/entrypoints-contracts.md#main-function) for more details on how to do this.

[Views](../contract/views.md) can be declared in a similar way with `@view` for JsLIGO and `[@view]` for CameLIGO.

As the use of `@entry` or `[@entry]` (and `@view` or `[@view]`) in the source is now mandatory, it is not possible anymore to rely on the auto-detection of the `main` function as the sole entry point, and it is not possible anymore to specify a entry points via the `-e` function on the command-line or views via the `--views` / `-v` options.

Another consequence of this change is that, when originating a contract for tests, `Test.originate` now take as an argument a module containing multiple entry points instead of a single function, i.e. a single entry point

We are also rolling out a new feature allowing the addition, removal and update of dynamic entry points for a contract after deployment. This could be a useful feature for example when building a DAO which allows on-chain vote to upgrade its code (or a DAO which controls the code of another separate contract). For more information, see [the documentation](../advanced/dynamic-entrypoints.md) and [the reference](../reference/dynamic_entrypoints.md) for this feature.

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2818
* https://gitlab.com/ligolang/ligo/-/merge_requests/2814
* https://gitlab.com/ligolang/ligo/-/merge_requests/2810
* https://gitlab.com/ligolang/ligo/-/merge_requests/2805
* https://gitlab.com/ligolang/ligo/-/merge_requests/2831
* https://gitlab.com/ligolang/ligo/-/merge_requests/2885

#### Uniform calling convention for views and entry points.

Views used to be functions taking a tuple, they are now functions taking two arguments:

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let set_storage (new_storage, _old_storage : int * int): operation list * int = ([], new_storage)

[@view]
let get_storage ((), storage : unit * int): int = storage
```

is now written

```cameligo
[@entry]
let set_storage (new_storage : int)  (_old_storage : int): operation list * int = ([], new_storage)

[@view]
let get_storage () (storage : int): int = storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
// @entry
const set_storage = ([new_storage, _old_storage] : [int, int]): [list<operation>, int] => [list([]), new_storage]

@view
const get_storage = ([_, storage] : [unit, int]): int => storage
```

is now written

```jsligo
@entry
const set_storage = (new_storage: int, _old_storage: int): [list<operation>, int] => [list([]), new_storage]

@view
const get_storage = (_: unit, storage: int): int => storage
```

</Syntax>


#### `contract_of` and `parameter_of`

The aforementioned changes to `@entry` and the `main` function have affected how contracts are tested, starting from [v0.64.2](https://gitlab.com/ligolang/ligo/-/releases/0.64.2) ([changelog](../intro/changelog.md#0642)). See [the documentation on testing](../advanced/testing.md#testing-a-contract-declared-as-a-module-or-namespace) for examples on how to use `contract_of` and `parameter_of`.

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2476
* https://gitlab.com/ligolang/ligo/-/merge_requests/2685

### `export` and `@private` now have the expected effect

Previously, all declarations would be exported regardless whether `export`, `@private` or neither was used.

In LIGO v1, JsLIGO definitions which are not marked with `export` are not exported, and CameLIGO definitions which are marked with `@private` are not exported. In other words, the default for JsLIGO is now to make definitions private unless specified otherwise with `export`, and the default for CameLIGO is now to make definitions public unless specified otherwise with `[@private]`.

Furthermore, in JsLIGO nested namespaces need to be exported in order to be accessed, e.g.

```jsligo group=namespace-export
namespace Foo {
    export namespace Bar {
        export const x = 1
    }
}

const y = Foo.Bar.x
```

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2796
* https://gitlab.com/ligolang/ligo/-/merge_requests/2684 in [v0.69.0](https://gitlab.com/ligolang/ligo/-/releases/0.69.0) ([changelog](../intro/changelog.md#0690))

### The comb layout is now used by default

Some types can have several isomorphic representations in Michelson, and LIGO allows choosing between two of these, `comb` and `tree`, via an `@layout` decorator (e.g. `@layout("comb")` in JsLIGO, or `[@layout comb]` in CameLIGO). See [the documentation on layouts](../advanced/interop#michelson-types-and-annotations).

Previously, the default layout was `tree`, and in LIGO v1, the default becomes `comb`.

The rationale is that the `comb` layout is usually more optimal, especially for records: records with a `comb` layout are compiled to Michelson combs, which have better support and look more readable. The `comb` layout is also more predictable / less surprising, because the fields are in declared order instead of alphabetical order. The `comb` layout can be less efficient for variants, but the difference should not be significant in most cases. For more info on why this change happened, see [Why did the default datatype layout change to `@layout comb`?](../faq/layout-comb-why.md)

If your project has a stable ABI that other tools rely on, you might need to manually annotate the type of entry point arguments and the entry point return types with `@layout("tree")` / `[@layout tree]`.

Once reaching the optimization phase of your development process, youu may wish to try annotating large variants (which contain many cases) with `@layout("tree")` / `[@layout tree]` and comparing the size and gas consumption of the compiled contracts.

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/1816.

### A small set of annotations / decorators are now supported

* `@entry`
* `@dyn_entry`
* `@inline`
* `@view`
* `@no_mutation`
* `@private`
* `@public`
* `@annot`
* `@layout`

These annotations / decorators should now be written without prefixing them with a comment, e.g.

```jsligo
@entry
const my_entry_point = (_: unit, n: int) : [list<operation>, int] => [list([]), n];
```

instead of

```jsligo skip
// @entry
const my_entry_point = (_: unit, n: int) : [list<operation>, int] => [list([]), n];
```

There are also two internal annotations / decorators, which should not appear in normal source code:

* `@thunk`
* `@hidden`

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2619 in [v0.67.0](https://gitlab.com/ligolang/ligo/-/releases/0.67.0) ([changelog](../intro/changelog.md#0670))


MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2476

### Field and tuple component access

Fields can be accessed with dot notation `stuff.y` and brackets `stuff["y"]` interchangeably:

```jsligo
const stuff = {
    x : "foo",
    y : { universe : [42, "life", true] }
};

const part : bool = stuff.y["universe"][2];
```

* https://gitlab.com/ligolang/ligo/-/merge_requests/2661

### Miscellaneous

* The internal command `ligo daemon` has been removed in [v0.69.0](https://gitlab.com/ligolang/ligo/-/releases/0.69.0) ([changelog](../intro/changelog.md#0690)). It was previously used by the old language server to create a persistent LIGO process, but it was hacky and offered no performance improvements. There should be no noticeable change for the user, as the new language server (used e.g. by the VsCode plug-in) does not make use of this command anymore. MR: https://gitlab.com/ligolang/ligo/-/merge_requests/2690.
* The support for CST mutation testing has been dropped in [v0.66.0](https://gitlab.com/ligolang/ligo/-/releases/0.66.0) ([changelog](../intro/changelog.md#0660)). Unfortunately, that feature was incomplete and broken. With the disappearance of this feature, the command `ligo mutate` has been removed. However, AST mutation testing is still supported and part of the [testing framework](../advanced/mutation-testing.md). MRs: https://gitlab.com/ligolang/ligo/-/merge_requests/2455 and https://gitlab.com/ligolang/ligo/-/merge_requests/2607.
* Starting from [v0.64.2](https://gitlab.com/ligolang/ligo/-/releases/0.64.2) ([changelog](../intro/changelog.md#0642)), the transpilation commands now take `--from-syntax` and `--to-syntax`, instead of the former, less clear use of `--syntax` for the source syntax and an unnamed parameter for the destination syntax. The destination syntax can still be inferred from the filename given to `-o`, e.g. `-o dest.jsligo`. MR: https://gitlab.com/ligolang/ligo/-/merge_requests/2501
* Starting from [v0.64.2](https://gitlab.com/ligolang/ligo/-/releases/0.64.2) ([changelog](../intro/changelog.md#0642)), the Kathmandu protocol is deprecated. If you need to recompile an old LIGO contract for an outdated protocol version, you may use the compiler version that the project was developed with. MR: https://gitlab.com/ligolang/ligo/-/merge_requests/2500

## JsLIGO

### Short notation for `tez` and `mutez`

You can now write `3tez` or `3mutez` instead of `3 as tez` or `3 as mutez`. This convenient feature was already present in CameLIGO and is now available in JsLIGO too!

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2853
* https://gitlab.com/ligolang/ligo/-/merge_requests/2661

### New bitwise operators

The following operators have been added, and can be used with `nat` and `bytes`.

* `&` Bitwise _and_
* `|` Bitwise _or_
* `^` Bitwise _xor_
* `<<` Bitwise left shift (the shift amount is always a `nat\, even when shifting `bytes`)
* `>>` Bitwise right shift (the shift amount is always a `nat\, even when shifting `bytes`)

Here are examples of these operators in context:

```jsligo
const zero: nat = 2n & 1n; // Bitwise and
const two_bytes : bytes = 0x11 & 0x10

const five: nat = 4n | 1n; // Bitwise or
const three_bytes : bytes = 0x11 | 0x10

const three : nat = 2n ^ 1n; // Bitwise xor
const one_byte : bytes = 0x11 ^ 0x10

const four : nat = 2n << 1n // Bitwise left shift
const five_one_two : bytes = 0x0100 << 1n

const one : nat = 2n >> 1n; // Bitwise right shift
const zero_bytes : bytes = 0x01 >> 1n
```

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2661

### Changes to pattern matching

JsLIGO's pattern matchin is inspired by the [ECMAScript Pattern Matching proposal](https://github.com/tc39/proposal-pattern-matching). This section covers some of the changes this implies.

The new `when` keyword makes pattern matching more explicit.

Furthermore, pattern matching is now a keyword, it is not anymore a function taking an object with cases as fields.

The `do { ... }` expression is equivalent to the `(() => { ... }) ()` thunk, i.e. it allows a block of code containing statements (like `const xyz = ...` or `return 42`) to be used where an expression is expected.

Therefore, a simple pattern matching like the following:

```jsligo skip
const force_positive = (key: string, dict: map<string, int>) => {
  return match(Map.find_opt (key, dict), {
    Some: (val : int) => {
        if (val >= 0) {
            return val;
        } else {
            failwith("Negative value.");
        }
    },
    None: () => failwith("Not found.")
  });
}
```

becomes:

```jsligo
const force_positive = (key: string, dict: map<string, int>) => {
  return match(Map.find_opt (key, dict)) {
    when(Some(val)): do {
        if (val >= 0) {
            return val;
        } else {
            failwith("Negative value");
        }
    };
    when(None()): failwith("Not found.")
  };
}
```

Pattern-matching on lists uses the syntaxes `when([])` and `when([head, ...tail])`:

```jsligo
type storage = [int, list <int>];
type parameter = list <int>;
type returnx = [list <operation>, storage];

let main = (p : parameter, s : storage) : returnx => {
  let storage = match (p) {
    when([]): s;
    when([hd, ...tl]): [s[0] + hd, tl]
  };
  return [(list([]) as list<operation>), storage];
};
```

Furthermore, there are a few changes to how patterns are written:

* Patterns for parameterless constructors take a `()` within the `when(...)`, therefore `Nil: () => 1` becomes `when(Nil()): 1`
* Patterns which match a constructor containing a tuple work similarly, e.g. `Cons: (pair) => pair.1 + f(pair.2)` becomes `when(Cons(pair)) => pair.1 + f(pair.2)`
* Patterns with one variable per parameter are written as expected: `Foo: (a, b) => a + b` becomes `when(Foo(a, b)): a + b`

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2661


### `_` is now a valid variable name and can't be used for its former throw-away semantics

Previously, following the tradition of some functional languages, `_` was used to discard the value bound to it, e.g.

```jsligo skip
// don't do this anymore
const f = () => {
  let _ = some_check();
  return match (foobar) {
    when([]) : "empty list";
    when([_, ..._]): "non-empty list";
  };
}
```

Instead, `_` is now a normal variable name, following the JavaScript and TypeScript tradition, where `_` is used as a short
name for a namespace containing many utilities, e.g. as an alias for the `lodash` library. This means that the code above should now assign unique names to the discarded value, like so:

```jsligo skip
// don't do this anymore
const f = () => {
  let _chk = some_check();
  return match (foobar) {
    when([]) : "empty list";
    when([_hd, ..._tl]): "non-empty list";
  };
}
```

If multiple _ variable are bound in the same scope, it will result in an error (duplicate block-scoped variable) just as in TypeScript. However, it is still possible to shadow a `_` within a smaller scope, e.g. if `_` is globally defined as an alias for another module, a function can still specify `_` as an argument name and shadow the global definition, which could cause issues. It is wise to skim over existing code for such cases.

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2674

### Imports are now automatically re-exported

When a module is imported e.g. with `#import "foo.jsligo" "Foo"` inside the file `bar.jsligo`, it is automatically re-exported.

For example, a third file importing `bar.jsligo` as `Bar` can write `Bar.Foo.x` to access the `x` defined in `foo.jsligo`

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2815

### Miscellaneous

* `true` and `false` are now keywords (not variables), and cannot be shadowed by a local variable declaration. https://gitlab.com/ligolang/ligo/-/merge_requests/2661

## CameLIGO

### Field and tuple component access

Fields and tuple components can be accessed with the same dot notation:

```cameligo
let stuff = {
    x = "foo";
    y = (42, "life", { universe = true });
}

let part : bool = stuff.y.2.universe
```

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2661

### Package management: use ligo.json instead of package.json or esy.json

Users often work with JaveScript toolchain alongside ours. Using package.json to manage both is tricky. It's better to have a separate manfiest to manage ligo dependencies. We therefore now use a separate `ligo.json` maninfest to manage LIGO packages.

As part of this change, we are no longer using the `esy` tool for package management, and the `installation.json` file, formerly located at `_esy/ligo/installation.json`, should now be moved to `_ligo/ligo/installation.json`.

MRs:
* https://gitlab.com/ligolang/ligo/-/merge_requests/2817
* https://gitlab.com/ligolang/ligo/-/merge_requests/2785
* https://gitlab.com/ligolang/ligo/-/merge_requests/2883

<!-- updated use of entry -->
