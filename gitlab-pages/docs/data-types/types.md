---
id: types
title: Data types
---

import Syntax from '@theme/Syntax';

**LIGO is strongly and statically typed.**

This means that the LIGO compiler must know what type each variable is to compile to Michelson, which is also strongly and statically typed.
The compiler also uses type information to check how contracts process data, ensuring that each function's expectations are met.
If it passes the test, your contract will not fail at run-time due to some inconsistent assumptions on your data.
This test is called *type checking*.

LIGO types are built on top of Michelson's type system, so there are many similarities but not a one-to-one match between LIGO types and Michelson types.
For information on Michelson types, see [Michelson: the language of Smart Contracts in Tezos](https://octez.tezos.com/docs/active/michelson.html) in the Octez documentation.

## Built-in types

For reference, you can find all the LIGO built-in types [here](https://gitlab.com/ligolang/ligo/-/blob/dev/src/main/build/ligo_lib/std_lib.mligo#L1-33).

## Type aliases

You can create aliases for types to give them more precise names, which can increase readability and maintainability of your code.
The following example creates an alias of the string type named `breed` to indicate variables that describe animal breeds.
Then it creates a function that accepts variables of that type:

<Syntax syntax="cameligo">

```cameligo group=a
type breed = string
let dog_breed_1 : breed = "Saluki"
let dog_breed_2 : string = "Shiba Inu"

let greet_dogs (dogs : breed list) : string =
  List.fold (fun (a, b : breed * breed) -> String.concats [a; ", "; b]) dogs "Hello"

let greeting = greet_dogs [dog_breed_1; dog_breed_2]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
type breed = string;
const dog_breed_1: breed = "Saluki";
const dog_breed_2: string = "Shiba Inu";

const greet_dogs = (dogs: list<breed>): string =>
  List.fold(([a, b]: [breed, breed]) => String.concats([a, ", ", b]), dogs, "Hello");
```

</Syntax>

:::note

Type aliases are not separate types.
Types and their aliases are interchangeable.
As shown in the previous example, you can pass any string to a function that accepts an alias of a string type.

:::

## Custom types

You can create custom types with the `type` keyword.
Then you can use those types in other places, as in this example, which creates a type to represent a map that uses addresses as keys and amounts of tez as values:

<Syntax syntax="cameligo">

```cameligo group=b
type account_balances = (address, tez) map

let ledger : account_balances =
  Map.literal
    [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 10mutez)]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
type account_balances = map<address, tez>;

const ledger : account_balances =
  Map.literal([["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, 10 as mutez]]);
```

</Syntax>

<!-- updated use of entry -->
