---
id: entrypoints-contracts
title: Main function and Entrypoints
---

## Access Functions

A LIGO contract is made of a series of constant and function
declarations. Only functions having a special type can be called when
the contract is activated: we call them *main functions*. A main
function takes two parameters, the *contract parameter* and the
*on-chain storage*, and returns a pair made of a *list of operations*
and a (new) storage.

When the contract is originated, the initial value of the storage is
provided. When a main function is later called, only the parameter is
provided, but the type of a main function contains both.

The type of the contract parameter and the storage are up to the
contract designer, but the type for list operations is not. The return
type of a main function is as follows, assuming that the type
`storage` has been defined elsewhere. (Note that you can use any type
with any name for the storage.)

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo skip
type storage is ...  // Any name, any type
type return is list (operation) * storage
```

<!--CameLIGO-->
```cameligo skip
type storage = ...  // Any name, any type
type return = operation list * storage
```

<!--ReasonLIGO-->
```reasonligo skip
type storage = ...;  // Any name, any type
type return = (list (operation), storage);
```
<!--END_DOCUSAURUS_CODE_TABS-->

The contract storage can only be modified by activating a main
function: given the state of the storage *on-chain*, a main function
specifies how to create another state for it, depending on the
contract's parameter.

Here is an example where the storage is a single natural number that
is updated by the parameter.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=a
type parameter is nat
type storage is nat
type return is list (operation) * storage

function save (const action : parameter; const store : storage) : return is
  ((nil : list (operation)), store)
```

<!--CameLIGO-->
```cameligo group=a
type parameter = nat
type storage = nat
type return = operation list * storage

let save (action, store: parameter * storage) : return =
  (([] : operation list), store)
```

<!--ReasonLIGO-->
```reasonligo group=a
type parameter = nat;
type storage = nat;
type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) : return =>
  (([] : list (operation)), store);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Entrypoints

In LIGO, the design pattern is to have *one* main function called
`main`, that dispatches the control flow according to its
parameter. Those functions called for those actions are called
*entrypoints*.

As an analogy, in the C programming language, the `main` function is
the unique main function and any function called from it would be an
entrypoint.

The parameter of the contract is then a variant type, and, depending
on the constructors of that type, different functions in the contract
are called. In other terms, the unique main function dispatches the
control flow depending on a *pattern matching* on the contract
parameter.

In the following example, the storage contains a counter of type `nat`
and a name of type `string`. Depending on the parameter of the
contract, either the counter or the name is updated.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=b
type parameter is
  Action_A of nat
| Action_B of string

type storage is record [
  counter : nat;
  name    : string
]

type return is list (operation) * storage

function entry_A (const n : nat; const store : storage) : return is
  ((nil : list (operation)), store with record [counter = n])

function entry_B (const s : string; const store : storage) : return is
  ((nil : list (operation)), store with record [name = s])

function main (const action : parameter; const store : storage): return is
  case action of
    Action_A (n) -> entry_A (n, store)
  | Action_B (s) -> entry_B (s, store)
  end
```

<!--CameLIGO-->
```cameligo group=b
type parameter =
  Action_A of nat
| Action_B of string

type storage = {
  counter : nat;
  name    : string
}

type return = operation list * storage

let entry_A (n, store : nat * storage) : return =
  ([] : operation list), {store with counter = n}

let entry_B (s, store : string * storage) : return =
  ([] : operation list), {store with name = s}

let main (action, store: parameter * storage) : return =
  match action with
    Action_A n -> entry_A (n, store)
  | Action_B s -> entry_B (s, store)
```

<!--ReasonLIGO-->
```reasonligo group=b
type parameter =
| Action_A (nat)
| Action_B (string);

type storage = {
  counter : nat,
  name    : string
};

type return = (list (operation), storage);

let entry_A = ((n, store): (nat, storage)) : return =>
  (([] : list (operation)), {...store, counter : n});

let entry_B = ((s, store): (string, storage)) : return =>
  (([] : list (operation)), {...store, name : s});

let main = ((action, store): (parameter, storage)) : return =>
  switch (action) {
  | Action_A (n) => entry_A ((n, store))
  | Action_B (s) => entry_B ((s, store))
  };
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Tezos-specific Built-ins

A LIGO smart contract can query part of the state of the Tezos
blockchain by means of built-in values. In this section you will find
how those built-ins can be utilized.

### Accepting or Declining Tokens in a Smart Contract

This example shows how `amount` and `failwith` can be used to decline
any transaction that sends more tez than `0mutez`, that is, no
incoming tokens are accepted.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
type parameter is unit
type storage is unit
type return is list (operation) * storage

function deny (const action : parameter; const store : storage) : return is
  if amount > 0mutez then
    (failwith ("This contract does not accept tokens.") : return)
  else ((nil : list (operation)), store)
```

<!--CameLIGO-->
```cameligo group=c
type parameter = unit
type storage = unit
type return = operation list * storage

let deny (action, store : parameter * storage) : return =
  if amount > 0mutez then
    (failwith "This contract does not accept tokens.": return)
  else (([] : operation list), store)
```

<!--ReasonLIGO-->
```reasonligo group=c
type parameter = unit;
type storage = unit;
type return = (list (operation), storage);

let deny = ((action, store): (parameter, storage)) : return => {
  if (amount > 0mutez) {
    (failwith("This contract does not accept tokens."): return); }
  else { (([] : list (operation)), store); };
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Access Control

This example shows how `sender` or `source` can be used to deny access to an entrypoint.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
const owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);

function main (const action : parameter; const store : storage) : return is
  if source =/= owner then (failwith ("Access denied.") : return)
  else ((nil : list(operation)), store)
```

<!--CameLIGO-->
```cameligo group=c
let owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)

let main (action, store: parameter * storage) : return =
  if source <> owner then (failwith "Access denied." : return)
  else (([] : operation list), store)
```

<!--ReasonLIGO-->
```reasonligo group=c
let owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);

let main = ((action, store): (parameter, storage)) : storage => {
  if (source != owner) { (failwith ("Access denied.") : return); }
  else { (([] : list (operation)), store); };
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Inter-Contract Invocations

It would be somewhat misleading to speak of "contract calls", as this
wording may wrongly suggest an analogy between contract "calls" and
function "calls". Indeed, the control flow returns to the site of a
function call, and composed function calls therefore are *stacked*,
that is, they follow a last in, first out ordering. This is not what
happens when a contract invokes another: the invocation is *queued*,
that is, follows a first in, first our ordering, and the dequeuing
only starts at the normal end of a contract (no failure). That is why
we speak of "contract invocations" instead of "calls".

The following example shows how a contract can invoke another by
emiting a transaction operation at the end of an entrypoint.

> The same technique can be used to transfer tokens to an implicit
> account (tz1, ...): all you have to do is use a unit value as the
> parameter of the smart contract.

In our case, we have a `counter.ligo` contract that accepts an action
of type `parameter`, and we have a `proxy.ligo` contract that accepts
the same parameter type, and forwards the call to the deployed counter
contract.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo skip
// counter.ligo
type parameter is
  Increment of nat
| Decrement of nat
| Reset

type storage is unit

type return is list (operation) * storage
```

```pascaligo group=d
// proxy.ligo

type parameter is
  Increment of nat
| Decrement of nat
| Reset

type storage is unit

type return is list (operation) * storage

const dest : address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" : address)

function proxy (const action : parameter; const store : storage): return is
  block {
    const counter : contract (parameter) = get_contract (dest);
    (* Reuse the parameter in the subsequent
       transaction or use another one, `mock_param`. *)
    const mock_param : parameter = Increment (5n);
    const op : operation = transaction (action, 0mutez, counter);
    const ops : list (operation) = list [op]
  } with (ops, store)
```

<!--CameLIGO-->
```cameligo skip
// counter.mligo

type parameter =
  Increment of nat
| Decrement of nat
| Reset

// ...
```

```cameligo group=d
// proxy.mligo

type parameter =
  Increment of nat
| Decrement of nat
| Reset

type storage = unit

type return = operation list * storage

let dest : address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" : address)

let proxy (action, store : parameter * storage) : return =
  let counter : parameter contract = Operation.get_contract dest in
  (* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. *)
  let mock_param : parameter = Increment (5n) in
  let op : operation = Operation.transaction action 0mutez counter
  in [op], store
```

<!--ReasonLIGO-->
```reasonligo skip
// counter.religo

type parameter =
| Increment (nat)
| Decrement (nat)
| Reset

// ...
```

```reasonligo group=d
// proxy.religo

type parameter =
| Increment (nat)
| Decrement (nat)
| Reset;

type storage = unit;

type return = (list (operation), storage);

let dest : address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" : address);

let proxy = ((action, store): (parameter, storage)) : return => {
  let counter : contract (parameter) = Operation.get_contract (dest);
  (* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. *)
  let mock_param : parameter = Increment (5n);
  let op : operation = Operation.transaction (action, 0mutez, counter);
  ([op], store)
};
```

<!--END_DOCUSAURUS_CODE_TABS-->
