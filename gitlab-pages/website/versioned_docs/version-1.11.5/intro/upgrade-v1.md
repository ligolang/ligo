---
title: Migrating to LIGO v1.0
---

import Syntax from '@theme/Syntax';

Version 1.0 of LIGO includes breaking changes, so you must update your source code when you upgrade your installation of LIGO as described in [Installation](./installation).

## Changes to the `main` entrypoint

<Syntax syntax="cameligo">

The LIGO compiler no longer auto-detects a function named `main` as the sole entrypoint of a contract.
Similarly, you can no longer specify entrypoints and views by passing functions to the `--entrypoint` and `--view` command-line arguments.
Now, use multiple entrypoints and views denoted with the `[@entry]` and `[@view]` attributes.

In the same way, the `Test.originate` function now takes as an argument a module containing multiple entry points instead of a single function.

Contracts can still have a single function named `main` decorated with `[@entry]` that manages all entrypoints for the contract, but this code structure is deprecated.
For more information and for workarounds to help you migrate contracts with a single `main` function, see [Entrypoints](../syntax/contracts/entrypoints) and [The main function](../syntax/contracts/entrypoints#the-main-function).

</Syntax>

<Syntax syntax="jsligo">

The LIGO compiler no longer auto-detects a function named `main` as the sole entrypoint of a contract.
Similarly, you can no longer specify entrypoints and views by passing functions to the `--entrypoint` and `--view` command-line arguments.
Now, use multiple entrypoints and views denoted with the `@entry` and `@view` decorators.

In the same way, the `Test.originate` function now takes as an argument a module containing multiple entry points instead of a single function.

Contracts can still have a single function named `main` decorated with `@entry` that manages all entrypoints for the contract, but this code structure is deprecated.
For more information and for workarounds to help you migrate contracts with a single `main` function, see [Entrypoints](../syntax/contracts/entrypoints) and [The main function](../syntax/contracts/entrypoints#the-main-function).

</Syntax>

## Changes to parameters for views and entrypoints

Views and entrypoints used to accept the parameter and current storage as a tuple.
Now, they accept these values as separate parameters.

<Syntax syntax="cameligo">

For example, this view uses the old syntax:

```cameligo skip
(* Prior to LIGO 1.0 *)
[@entry]
let set_storage (new_storage, _old_storage : int * int): operation list * int = ([], new_storage)

[@view]
let get_storage ((), storage : unit * int): int = storage
```

This equivalent view uses the new syntax:

```cameligo
[@entry]
let set_storage (new_storage : int)  (_old_storage : int): operation list * int = ([], new_storage)

[@view]
let get_storage () (storage : int): int = storage
```

</Syntax>

<Syntax syntax="jsligo">

For example, this view uses the old syntax:

```jsligo skip
// Prior to LIGO 1.0
// @entry
const set_storage = ([new_storage, _old_storage] : [int, int]): [list<operation>, int] => [[], new_storage]

@view
const get_storage = ([_, storage] : [unit, int]): int => storage
```

This equivalent view uses the new syntax:

```jsligo
@entry
const set_storage = (new_storage: int, _old_storage: int): [list<operation>, int] => [[], new_storage]

@view
const get_storage = (_: unit, storage: int): int => storage
```

</Syntax>

## Changes to `contract_of` and `parameter_of`

The aforementioned changes to entrypoints, views, and the `main` function affect how contracts are tested.
Beginning in version 0.64.2, the behavior of `contract_of` and `parameter_of` in tests has changed.

For examples of how to use `contract_of`, see [Testing](../testing).
For examples of how to use `parameter_of`, see [Contracts](../data-types/contracts-type).

## `export` and `@private` now have the expected effect

Previously, all declarations were exported regardless whether `export`, `@private` or neither was used.

In LIGO v1.0, JsLIGO definitions which are not marked with `export` or designated as an entrypoint are not exported, and CameLIGO definitions which are marked with `@private` are not exported.
In other words, the default for JsLIGO is now to make definitions private unless specified otherwise with `export` of `@entry`, and the default for CameLIGO is now to make definitions public unless specified otherwise with `[@private]`.

Furthermore, in JsLIGO nested namespaces need to be exported in order to be accessed.
For example, this code accesses a declaration that is inside another module; this is possible only because the declaration is exported:

```jsligo group=namespace-export
namespace Foo {
    export namespace Bar {
        export const x = 1
    }
}

const y = Foo.Bar.x
```

## The comb layout is now used by default

LIGO can format types in Michelson in two basic layouts: combs and trees.
Previously, LIGO used trees by default, but in LIGO v1, it uses combs by default.

As described in [Interoperability](../syntax/contracts/interop), sometimes you need to match the layout of Michelson values, such as when you call a contract that accepts parameters in combs or trees.
You can use the `@layout` decorator in JsLIGO or the `[@layout]` attribute in CameLIGO to set the layout of a type.

For more info on why this change happened, see [Why did the default datatype layout change to `@layout comb`?](/blog/layout-comb-why)

If your project has a stable ABI that other tools rely on, you might need to manually set the type of entrypoint arguments and return types with the `@layout` decorator in JsLIGO or the `[@layout]` attribute in CameLIGO.

Combs and trees can perform differently, so you can try setting types such as variants with many cases as combs or trees and comparing the size and gas consumption of the compiled contracts.

## Decorators are not commented

Prior to version 1.0, JsLIGO decorators were put in comments, as in this example:

```jsligo skip
// @entry
const my_entry_point = (_: unit, n: int) : [list<operation>, int] => [[], n];
```

Now, decorators are not in comments, as in this example:

```jsligo
@entry
const my_entry_point = (_: unit, n: int) : [list<operation>, int] => [[], n];
```

For more information, see [Decorators](../syntax/decorators).

## Field and tuple component access

Fields can be accessed with dot notation `stuff.y` and brackets `stuff["y"]` interchangeably:

```jsligo
const stuff = {
    x : "foo",
    y : { universe : [42, "life", true] }
};

const part : bool = stuff.y["universe"][2];
```

## Changes to CLI commands

- The internal command `ligo daemon` was removed in v0.69.0 ([changelog](./changelog#0690)).
There should be no noticeable change for the user because the new language server (used by tools such as the VsCode plug-in) does not use this command any more.

- Starting from v0.64.2 ([changelog](./changelog#0642)), the transpilation commands now take the arguments `--from-syntax` and `--to-syntax` instead of the argument `--syntax` for the source syntax and an unnamed parameter for the destination syntax.
The destination syntax can still be inferred from the filename given to the `-o` argument, such as `-o dest.jsligo`.

- The command `ligo mutate` has been removed; use [Mutation testing](../testing/mutation-testing) instead.

## Mutation testing

Support for CST mutation testing was dropped in v0.66.0 ([changelog](./changelog#0660)).
However, AST mutation testing is still supported; see [Mutation testing](../testing/mutation-testing).

## Kathmandu protocol deprecated

Starting from v0.64.2 ([changelog](./changelog#0642)), the Kathmandu protocol is deprecated.
If you need to recompile an old LIGO contract for an outdated protocol version, you can use the compiler version that the project was developed with.

## Package management: use ligo.json instead of package.json or esy.json

To separate LIGO package management from JavaScript package management toolchains such as Node.JS, LIGO now uses a separate manifest in the file `ligo.json`.

As part of this change, LIGO no longer uses the `esy` tool for package management, and the `installation.json` file, formerly located at `_esy/ligo/installation.json`, should now be moved to `_ligo/ligo/installation.json`.

## Changes specific to JsLIGO

### Short notation for `tez` and `mutez`

You can now write `3tez` or `3mutez` instead of `3 as tez` or `3 as mutez`.

### New bitwise operators

The following operators have been added, and can be used with `nat` and `bytes`:

- `&`: Bitwise _and_
- `|`: Bitwise _or_
- `^`: Bitwise _xor_
- `<<`: Bitwise left shift (the shift amount is always a `nat\, even when shifting `bytes`)
- `>>`: Bitwise right shift (the shift amount is always a `nat\, even when shifting `bytes`)

Here are examples of these operators in context:

```jsligo
const zero: nat = 2n & 1n; // Bitwise and
const two_bytes: bytes = 0x11 & 0x10

const five: nat = 4n | 1n; // Bitwise or
const three_bytes: bytes = 0x11 | 0x10

const three: nat = 2n ^ 1n; // Bitwise xor
const one_byte: bytes = 0x11 ^ 0x10

const four: nat = 2n << 1n // Bitwise left shift
const five_one_two: bytes = 0x0100 << 1n

const one: nat = 2n >> 1n; // Bitwise right shift
const zero_bytes: bytes = 0x01 >> 1n
```

For more information, see [Bitwise operations](../data-types/bytes#bitwise-operations).

### Changes to pattern matching

JsLIGO's pattern matching is inspired by the [ECMAScript Pattern Matching proposal](https://github.com/tc39/proposal-pattern-matching).
This section covers some of the changes this implies:

The new `when` keyword makes pattern matching more explicit.
Specifically, pattern matching uses the `when` keyword instead of a function that takes an object with cases as fields.

The `do { ... }` expression is equivalent to the `(() => { ... }) ()` thunk.
It allows a block of code containing statements (like `const xyz = ...` or `return 42`) to be used where an expression is expected.

For example, this is a pattern match prior to JsLIGO 1.0:

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

This is an equivalent match in JsLIGO 1.0:

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

Pattern-matching on lists in version 1.0 uses the syntaxes `when([])` and `when([head, ...tail])`:

```jsligo
type storage = [int, list <int>];
type parameter = list <int>;
type returnx = [list <operation>, storage];

let main = (p : parameter, s : storage) : returnx => {
  let storage = match (p) {
    when([]): s;
    when([hd, ...tl]): [s[0] + hd, tl]
  };
  return [([] as list<operation>), storage];
};
```

Furthermore, there are a few changes to how patterns are written:

- Patterns for parameterless constructors take a `()` within the `when(...)`; for example `Nil: () => 1` becomes `when(Nil()): 1`
- Patterns that match a constructor containing a tuple work similarly; for example `Cons: (pair) => pair.1 + f(pair.2)` becomes `when(Cons(pair)) => pair.1 + f(pair.2)`
- Patterns with one variable per parameter are written as expected: `Foo: (a, b) => a + b` becomes `when(Foo(a, b)): a + b`

For more information, see [Matching](../data-types/variants#matching).

### A single underscore is now a valid variable name

Previously, following the convention of some functional languages, you could use a single underscore character (`_`) to discard the value bound to it, as in this example:

```jsligo skip
// Prior to JsLIGO 1.0
const f = () => {
  let _ = some_check();
  return match (foobar) {
    when([]) : "empty list";
    when([_, ..._]): "non-empty list";
  };
}
```

Instead, a single underscore is now a normal variable name, following the JavaScript and TypeScript convention, where the underscore is used as a short name for a namespace containing many utilities, such as an alias for the `lodash` library.
This means that the code above should now assign unique names to the discarded value, as in this example:

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

If multiple variables named with a single underscore are declared in the same scope, the compiler now throws an error (duplicate block-scoped variable) just as in TypeScript.
However, it is still possible to shadow a variable named `_` within a smaller scope.
For example, if the variable is globally defined as an alias for another module, a function can still specify a variable named `_` as an argument name and shadow the global definition.
Shadowing variables in this way can cause issues and therefore you should review your code for such cases.

### Imports are now automatically re-exported

When you import a module with the `#import` directive, it is automatically re-exported.
For example, if a file imports the file `foo.jsligo` with the code `#import "foo.jsligo" "Foo"`, a third file importing `bar.jsligo` as `Bar` can write `Bar.Foo.x` to access the `x` defined in `foo.jsligo`.

For more information, see [`#import`](../compiling/preprocessor#import).

### `true` and `false` are reserved words

`true` and `false` are now keywords, not variables, and cannot be shadowed by a local variable declaration.

## Changes specific to CameLIGO

### Field and tuple component access

Fields and tuple components can be accessed with the same dot notation:

```cameligo
let stuff = {
    x = "foo";
    y = (42, "life", { universe = true });
}

let part : bool = stuff.y.2.universe
```
