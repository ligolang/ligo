---
id: composite-types
title: Composite Types
---

import Syntax from '@theme/Syntax';

## Structured types

Often contracts require complex data structures, which in turn require
well-typed storage or functions to work with. LIGO offers a simple way
to compose simple types into *structured types*.

The first of those structured types is the *record*, which aggregates
types as *fields* and indexes them with a *field name*. In the example
below you can see the definition of data types for a ledger that keeps
the balance and number of previous transactions for a given account.

<Syntax syntax="cameligo">

```cameligo group=c
// Type aliasing

type account = address
type number_of_transactions = nat

// The type account_data is a record with two fields.

type account_data = {
  balance : tez;
  transactions : number_of_transactions
}

// A ledger is a map from accounts to account_data

type ledger = (account, account_data) map

let my_ledger : ledger = Map.literal
  [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address),
    {balance = 10mutez; transactions = 5n})]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
// Type aliasing

type account = address;
type number_of_transactions = nat;

// The type account_data is a record with two fields.

type account_data = {
  balance: tez,
  transactions: number_of_transactions
};

// A ledger is a map from accounts to account_data

type ledger = map <account, account_data>;

const my_ledger : ledger =
  Map.literal(list([
    ["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address,
     {balance: 10mutez, transactions: 5n}]]));
```

</Syntax>

Complementary to records are the *variant types*, which are described in the
section on [pattern matching](https://ligolang.org/docs/language-basics/unit-option-pattern-matching#variant-types).
Records are a product of types, while variant types are sums of types.

<!-- updated use of entry -->