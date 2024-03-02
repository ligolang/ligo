---
id: types
title: Simple Types
---

import Syntax from '@theme/Syntax';

*LIGO is strongly and statically typed.* This means that the compiler
checks how your contract processes data, ensuring that each function's
expectations are met. If it passes the test, your contract will not fail at
run-time due to some inconsistent assumptions on your data. This is
called *type checking*.

LIGO types are built on top of Michelson's type system.

## Built-in types

For quick reference, you can find all the built-in types [here](https://gitlab.com/ligolang/ligo/-/blob/dev/src/main/build/ligo_lib/std_lib.mligo#L1-33).

## Type aliases

*Type aliasing* consists of renaming a given type when the context
calls for a more precise name. This increases readability and
maintainability of your smart contracts. For example we can choose to
alias a string type as an animal breed - this will allow us to
communicate our intent with added clarity.

<Syntax syntax="cameligo">

```cameligo group=a
type breed = string
let dog_breed : breed = "Saluki"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
type breed = string;
const dog_breed: breed = "Saluki";
```

</Syntax>


> The above type definitions are aliases, which means that `breed` and
> `string` are interchangeable in all contexts.

## Simple types

<Syntax syntax="cameligo">

```cameligo group=b
// The type account_balances denotes maps from addresses to tez

type account_balances = (address, tez) map

let ledger : account_balances =
  Map.literal
    [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 10mutez)]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
// The type account_balances denotes maps from addresses to tez

type account_balances = map<address, tez>;

const ledger: account_balances =
  Map.literal
    (list([["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, 10mutez]]));
```

</Syntax>

<!-- updated use of entry -->