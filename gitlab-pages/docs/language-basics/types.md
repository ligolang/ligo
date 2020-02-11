---
id: types
title: Types
---

*LIGO is strongly and statically typed.* This means that the compiler
checks how your contract processes data. If it passes the test, your
contract will not fail at run-time due to inconsistent assumptions on
your data. This is called *type checking*.

LIGO types are built on top of Michelson's type system.

## Built-in types

For quick reference, you can find all the built-in types [here](https://gitlab.com/ligolang/ligo/blob/dev/src/passes/operators/operators.ml#L35).

## Type aliases

*Type aliasing* consists in renaming a given type, when the context
calls for a more precise name. This increases readability and
maintainability of your smart contracts. For example we can choose to
alias a string type as an animal breed - this will allow us to
comunicate our intent with added clarity.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
type breed is string
const dog_breed : breed = "Saluki"
```

<!--CameLIGO-->

```cameligo group=a
type breed = string
let dog_breed : breed = "Saluki"
```

<!--ReasonLIGO-->

```reasonligo group=a
type breed = string;
let dog_breed : breed = "Saluki";
```

<!--END_DOCUSAURUS_CODE_TABS-->

> The above type definitions are aliases, which means that `breed` and
> `string` are interchangable in all contexts.

## Simple types

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=b
// The type account_balances denotes maps from addresses to tez

type account_balances is map (address, tez)

const ledger : account_balances =
  map [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> 10mutez]
```

<!--CameLIGO-->
```cameligo group=b
// The type account_balances denotes maps from addresses to tez

type account_balances = (address, tez) map

let ledger : account_balances =
  Map.literal
    [(("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), 10mutez)]
```

<!--ReasonLIGO-->
```reasonligo group=b
// The type account_balances denotes maps from addresses to tez

type account_balances = map (address, tez);

let ledger: account_balances =
  Map.literal
    ([("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, 10mutez)]);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Structured types

Often contracts require complex data structures, which in turn require
well-typed storage or functions to work with. LIGO offers a simple way
to compose simple types into *structured types*.

The first of those structured types is the *record*, which aggregates
types as *fields* and index them with a *field name*. In the example
below you can see the definition of data types for a ledger that keeps
the balance and number of previous transactions for a given account.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
// Type aliasing

type account is address
type number_of_transactions is nat

// The type account_data is a record with two fields.

type account_data is record [
  balance : tez;
  transactions : number_of_transactions
]

// A ledger is a map from accounts to account_data

type ledger is map (account, account_data)

const my_ledger : ledger = map [
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) ->
  record [
    balance = 10mutez;
    transactions = 5n
  ]
]
```

<!--CameLIGO-->
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

<!--ReasonLIGO-->
```reasonligo group=c
// Type aliasing

type account = address;
type number_of_transactions = nat;

// The type account_data is a record with two fields.

type account_data = {
  balance : tez,
  transactions : number_of_transactions
};

// A ledger is a map from accounts to account_data

type ledger = map (account, account_data);

let my_ledger : ledger =
  Map.literal([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address,
     {balance: 10mutez, transactions: 5n})]);
```

The structured types which are dual to records are the *variant types*
and they are described in the section about *pattern matching*. They
are dual because records are a product of types (types are bundled
into a record), whereas variant types are a sum of types (they are
exclusive to each other).

<!--END_DOCUSAURUS_CODE_TABS-->

## Annotations

In certain cases, the type of an expression cannot be properly
inferred by the compiler. In order to help the type checker, you can
annotate an expression with its desired type. Here is an example:

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=d
type parameter is Back | Claim | Withdraw

type storage is
  record
    owner    : address;
    goal     : tez;
    deadline : timestamp;
    backers  : map (address, tez);
    funded   : bool
  end

type return is list (operation) * storage

function back (var action : unit; var store : storage) : return is
  begin
    if now > store.deadline then
      failwith ("Deadline passed.")
    else case store.backers[sender] of
           None -> store.backers[sender] := amount
         | Some (x) -> skip
         end
  end with ((nil : list (operation)), store) // Annotation
```

<!--CameLIGO-->
```cameligo group=d
type parameter = Back | Claim | Withdraw

type storage = {
  owner    : address;
  goal     : tez;
  deadline : timestamp;
  backers  : (address, tez) map;
  funded   : bool
}

type return = operation list * storage

let back (param, store : unit * storage) : return =
  let no_op : operation list = [] in
  if Current.time > store.deadline then
    (failwith "Deadline passed." : return) // Annotation
  else
    match Map.find_opt sender store.backers with
      None ->
        let backers = Map.update sender (Some amount) store.backers
        in no_op, {store with backers=backers}
    | Some (x) -> no_op, store
```

<!--ReasonLIGO-->
```reasonligo group=d
type parameter = | Back | Claim | Withdraw;

type storage = {
  owner    : address,
  goal     : tez,
  deadline : timestamp,
  backers  : map (address, tez),
  funded   : bool,
};

type return = (list (operation), storage);

let back = ((param, store) : (unit, storage)) : return => {
  let no_op : list (operation) = [];
  if (Current.time > store.deadline) {
    (failwith ("Deadline passed.") : return); // Annotation
  }
  else {
    switch (Map.find_opt (sender, store.backers)) {
    | None => {
        let backers = Map.update (sender, Some (amount), store.backers);
        (no_op, {...store, backers:backers}) }
    | Some (x) => (no_op, store)
    }
  }
};
```

<!--END_DOCUSAURUS_CODE_TABS-->
