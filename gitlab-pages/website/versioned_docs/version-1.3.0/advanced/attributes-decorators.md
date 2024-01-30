---
id: decorators
title: Attributes or Decorators
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

# Attributes

Attributes (also known as "decorators" in JsLIGO, annotations or pragmas in other languages) are markers that can affect how the code immediately after is compiled or interpreted. Depending on the attribute, that code can be a definition, variable, statement block and so on. The sections below describe the attributes supported by CameLIGO.

It is possible to specify multiple attributes as follows:

```cameligo
type storage = int
type result = operation list * storage

[@entry] [@no_mutation]
let sub (delta : int) (store : storage) : result =
  [], store - delta
```

## @entry to mark a function as a Smart Contract entry point

The `@entry` attribute is used to indicate that a function should be available as an entry point of the smart contract. It is essentially declaring that function as one of the several "main" functions. For more information, see [Main functions and Entrypoints](../advanced/entrypoints-contracts.md)

## @dyn_entry to mark a function as a dynamic entry point

The `@dyn_entry` attribute is used to indicate that a function should be available as a dynamic entry point of the smart contract. Dynamic entry points can be removed or updated without deploying a new contract, unlike entry points marked with `@entry`, which cannot be modified after the contract is originated. Dynamic entry points can be used for example to implement a DAO (Decentralized Autonomous Organization) with a built-in update mechanism that allows participants to vote on upgrades to the contract, somewhat akin to the process which allows to amend the Tezos protocol. For more information, see [Dynamic entrypoints](../advanced/dynamic-entrypoints.md).

## @inline to ensure a function is inlined at its call sites

The `@inline` attribute indicates that the code of the function it annotates must be inlined wherever it is called. This allows some optimizations to be performed, possibly at the expense of a larger compiled code. Benchmarks and profiling can help decide whether a function should be inlined or nor. For more information, see [Inlining](../advanced/inline.md)

## @view to mark a function as a Smart Contract on-chain view

Views are a form of read-only entry points, that can be called synchronously. For more information, see [LIGO views](../contract/views.md).

## @no_mutation

See [the documentation on mutation testing](../advanced/mutation-testing.md#preventing-mutation).

## @private to prevent an identifier from being accessed outside of its defining module

The `[@private]` decorator can be used on a top-level declaration, to prevent that value from being used outside the declaring module.

```cameligo group=module-with-private
(* This is gitlab-pages/docs/advanced/src/attributes-decorators/module-with-private.mligo *)
[@private] let stuff = 42
[@private] let g x = x * stuff
let f x = (g x) + 1
```

```cameligo group=import-module-with-private
(* This is gitlab-pages/docs/advanced/src/attributes-decorators/import-module-with-private.mligo *)
#import "gitlab-pages/docs/advanced/src/attributes-decorators/module-with-private.mligo" "ModuleWithPrivate"

(* foo = 5167 = (123 * 42) + 1 *)
let foo = ModuleWithPrivate.f 123

(*
  The following lines cause errors because g and stuff are private:

  let bad_1 = ModuleWithPrivate.g 123
  let bad_2 = ModuleWithPrivate.stuff
*)
```

## @annot to ornate the generated code with Michelson annotations

This attribute can be used to choose the name of the Michelson equivalent of record fields or variant constructors. By default, a variant is compiled to a comb of Michelson `or`, and each leaf in the nested tree of `or` that corresponds to a constructor is annotated with that constructor's name. Similarly, a record is compiled to a comb of Michelson `pair`, and each leaf in the nested tree of `pair` that corresponds to a field is annotated with that field's name.

Using `@annot`, the Michelson annotation for a given field or constructor can be customized. This is useful for interoperability, where a third-party program or contract expects specific Michelson annotations, even if the LIGO code might not use these names internally.

For more information, see [Interop: Different Michelson annotations](../advanced/interop.md#different-michelson-annotations)?

## @layout to specify the Michelson layout of composite data types (structures and variants)

Michelson does not natively support records or variants. These have to be encoded using nested `pair`s or nested `or`s. Many tree representations could translate to the same linear sequence of fields or constructors. LIGO makes it possible to choose between a right comb which preserves the order or the fields or constructors as declared in the source code, and a left-balanced, alphabetically ordered binary tree. The attributes `[@layout comb]` and `[@layout tree]` can be placed before the `{ ... }` for records and before the first constructor or leading `|` for variants, in order to explicitly choose the desired layout.

For more information, see [Interop: Michelson layout of LIGO data structures](../advanced/interop.md##michelson-layout-of-ligo-data-structures).

## Internal attributes

Furthermore, the following attributes are used internally by the compiler, you may encounter them when exporting the AST after a certain compilation pass, but they should not appear in normal source code.

* `@thunk`
* `@hidden`
* `@public`
* `@deprecated`

</Syntax>

<Syntax syntax="jsligo">

# Decorators

Decorators (also known as "attributes" in CameLIGO, annotations or pragmas in other languages) are markers that can affect how the code immediately after is compiled or interpreted. Depending on the decorator, that code can be a definition, variable, statement block and so on. The sections below describe the decorators supported by JsLIGO.

It is possible to specify multiple decorators as follows:

```jsligo
type storage = int;
type result = [list<operation>, storage];

@entry @no_mutation
const sub = (delta : int, store : storage) : result =>
  [list([]), store - delta]
```

## @entry to mark a function as a Smart Contract entry point

The `@entry` decorator is used to indicate that a function should be available as an entry point of the smart contract. It is essentially declaring that function as one of the several "main" functions. For more information, see [Main functions and Entrypoints](../advanced/entrypoints-contracts.md)

## @dyn_entry to mark a function as a dynamic entry point

The `@dyn_entry` decorator is used to indicate that a function should be available as a dynamic entry point of the smart contract. Dynamic entry points can be removed or updated without deploying a new contract, unlike entry points marked with `@entry`, which cannot be modified after the contract is originated. Dynamic entry points can be used for example to implement a DAO (Decentralized Autonomous Organization) with a built-in update mechanism that allows participants to vote on upgrades to the contract, somewhat akin to the process which allows to amend the Tezos protocol. For more information, see [Dynamic entrypoints](../advanced/dynamic-entrypoints.md).

## @inline to ensure a function is inlined at its call sites

The `@inline` decorator indicates that the code of the function it annotates must be inlined wherever it is called. This allows some optimizations to be performed, possibly at the expense of a larger compiled code. Benchmarks and profiling can help decide whether a function should be inlined or nor. For more information, see [Inlining](../advanced/inline.md)

## @view to mark a function as a Smart Contract on-chain view

Views are a form of read-only entry points, that can be called synchronously. For more information, see [LIGO views](../contract/views.md).

## @no_mutation

See [the documentation on mutation testing](../advanced/mutation-testing.md#preventing-mutation).

## @private to prevent an identifier from being accessed outside of its defining module

This decorator is meant to be used in CameLIGO, where definitions are public/exported by default. It has no use in JsLIGO.

## @annot to ornate the generated code with Michelson annotations

This decorator can be used to choose the name of the Michelson equivalent of record fields or variant constructors. By default, a variant is compiled to a comb of Michelson `or`, and each leaf in the nested tree of `or` that corresponds to a constructor is annotated with that constructor's name. Similarly, a record is compiled to a comb of Michelson `pair`, and each leaf in the nested tree of `pair` that corresponds to a field is annotated with that field's name.

Using `@annot`, the Michelson annotation for a given field or constructor can be customized. This is useful for interoperability, where a third-party program or contract expects specific Michelson annotations, even if the LIGO code might not use these names internally.

For more information, see [Interop: Different Michelson annotations](../advanced/interop.md#different-michelson-annotations)?

## @layout to specify the Michelson layout of composite data types (structures and variants)

Michelson does not natively support records or variants. These have to be encoded using nested `pair`s or nested `or`s. Many tree representations could translate to the same linear sequence of fields or constructors. LIGO makes it possible to choose between a right comb which preserves the order or the fields or constructors as declared in the source code, and a left-balanced, alphabetically ordered binary tree. The decorators `@layout("comb")` and `@layout("tree")` can be placed before the `{ ... }` for records and before the first constructor or leading `|` for variants, in order to explicitly choose the desired layout.

For more information, see [Interop: Michelson layout of LIGO data structures](../advanced/interop.md##michelson-layout-of-ligo-data-structures).

## export to mark an identifier as accessible outside its defining module

This decorator is slightly different from the others in that it does not need `@` and simply appears as follows:

```jsligo
namespace MathPi {
  export const pi_millionth_numerator : int = 3141593
  export const pi_millionth_denominator : nat = 1000000n
}
```

## Internal decorators

Furthermore, the following attributes are used internally by the compiler, you may encounter them when exporting the AST after a certain compilation pass, but they should not appear in normal source code.

* `@thunk`
* `@hidden`
* `@public`
* `@deprecated`

</Syntax>
