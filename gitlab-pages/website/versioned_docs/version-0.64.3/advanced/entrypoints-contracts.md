---
id: entrypoints-contracts
title: Main function and Entrypoints
---

import Syntax from '@theme/Syntax';

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

<Syntax syntax="pascaligo">

```pascaligo skip
type storage is ...  // Any name, any type
type return is list (operation) * storage
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
type storage = ...  // Any name, any type
type return = operation list * storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
type storage = ...;  // Any name, any type
type return_ = [list<operation>, storage];
```

</Syntax>

The contract storage can only be modified by activating a main
function: given the state of the storage *on-chain*, a main function
specifies how to create another state for it, depending on the
contract's parameter.

Here is an example where the storage is a single natural number that
is updated by the parameter.

<Syntax syntax="pascaligo">

```pascaligo group=a
type parameter is nat
type storage is nat
type return is list (operation) * storage

function save (const action : parameter; const store : storage) : return is
  (nil, store)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
type parameter = nat
type storage = nat
type return = operation list * storage

let main (action : parameter) (store : storage) : return =
  ([], store)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
type parameter = nat;
type storage = nat;
type return_ = [list<operation>, storage];

const main = (action: parameter, store: storage): return_ =>
  [list([]), store];
```

</Syntax>

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

<Syntax syntax="pascaligo">

```pascaligo group=b
type parameter is
  Action_A of nat
| Action_B of string

type storage is
  record [
    counter : nat;
    name    : string
  ]

type return is list (operation) * storage

function entry_A (const n : nat; const store : storage) : return is
  (nil, store with record [counter = n])

function entry_B (const s : string; const store : storage) : return is
  (nil, store with record [name = s])

function main (const action : parameter; const store : storage): return is
  case action of [
    Action_A (n) -> entry_A (n, store)
  | Action_B (s) -> entry_B (s, store)
  ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=b
type parameter =
  Action_A of nat
| Action_B of string

type storage = {
  counter : nat;
  name    : string
}

type return = operation list * storage

let entry_A (n : nat) (store : storage) : return =
  [], {store with counter = n}

let entry_B (s : string) (store : storage) : return =
  [], {store with name = s}

let main (action : parameter) (store: storage) : return =
  match action with
    Action_A n -> entry_A n store
  | Action_B s -> entry_B s store
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
type parameter =
| ["Action_A", nat]
| ["Action_B", string];

type storage = {
  counter : nat,
  name    : string
};

type return_ = [list<operation>, storage];

const entry_A = (n: nat, store: storage): return_ =>
  [list([]), {...store, counter: n}];

const entry_B = (s: string, store: storage): return_ =>
  [list([]), {...store, name: s}];

const main = (action: parameter, store: storage): return_ =>
  match(action, {
    Action_A: n => entry_A(n, store),
    Action_B: s => entry_B(s, store)
  });
```

</Syntax>

## Tezos-specific Built-ins

A LIGO smart contract can query part of the state of the Tezos
blockchain by means of built-in values. In this section you will find
how those built-ins can be utilised.

### Accepting or Declining Tokens in a Smart Contract

This example shows how `Tezos.get_amount` and `failwith` can be used to
decline any transaction that sends more tez than `0tez`, that is, no
incoming tokens are accepted.

<Syntax syntax="pascaligo">

```pascaligo group=c
type parameter is unit
type storage is unit
type return is list (operation) * storage

function deny (const action : parameter; const store : storage) : return is
  if Tezos.get_amount() > 0tez then
    failwith ("This contract does not accept tokens.")
  else (nil, store)
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=c
type parameter = unit
type storage = unit
type return = operation list * storage

let deny (action : parameter) (store : storage) : return =
  if Tezos.get_amount () > 0tez then
    failwith "This contract does not accept tokens."
  else ([], store)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
type parameter = unit;
type storage = unit;
type return_ = [list<operation>, storage];

const deny = (action: parameter, store: storage): return_ => {
  if (Tezos.get_amount() > (0 as tez)) {
    return failwith("This contract does not accept tokens.");
  } else {
    return [list([]), store];
  };
};
```

</Syntax>

### Access Control

This example shows how `Tezos.get_sender` can be used to deny access to an
entrypoint.

<Syntax syntax="pascaligo">

```pascaligo group=c
const owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);

function main (const action : parameter; const store : storage) : return is
  if Tezos.get_sender() =/= owner then failwith ("Access denied.")
  else (nil, store)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=c
let owner = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)

let main (action : parameter) (store: storage) : return =
  if Tezos.get_sender () <> owner then failwith "Access denied."
  else ([], store)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
const owner = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;

const main = (action: parameter, store: storage): return_ => {
  if (Tezos.get_sender() != owner) { return failwith("Access denied."); }
  else { return [list([]), store]; };
};
```

</Syntax>

> Note that we do not use `Tezos.get_source`, but instead
> `Tezos.get_sender`. In our [tutorial about
> security](../tutorials/security/security.md#incorrect-authorisation-checks)
> you can read more about it.

### Inter-Contract Invocations

It would be somewhat misleading to speak of "contract calls", as this
wording may wrongly suggest an analogy between contract "calls" and
function "calls". Indeed, the control flow returns to the site of a
function call, and composed function calls therefore are *stacked*,
that is, they follow a last in, first out ordering. This is not what
happens when a contract invokes another: the invocation is *queued*,
that is, follows a first in, first out ordering, and the dequeuing
only starts at the normal end of a contract (no failure). That is why
we speak of "contract invocations" instead of "calls".

The following example shows how a contract can invoke another by
emitting a transaction operation at the end of an entrypoint.

> The same technique can be used to transfer tokens to an implicit
> account (tz1, ...): all you have to do is use a unit value as the
> parameter of the smart contract.

In our case, we have a `counter` contract that accepts an action of
type `parameter`, and we have a `proxy` contract that accepts the same
parameter type, and forwards the call to the deployed counter
contract.

<Syntax syntax="pascaligo">

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

function proxy (const action : parameter; const store : storage): return is {
  const counter : contract (parameter) =
    case (Tezos.get_contract_opt (dest) : option (contract (parameter))) of [
      Some (contract) -> contract
    | None -> failwith ("Contract not found.")
    ];

  (* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. *)

  const mock_param : parameter = Increment (5n);
  const op = Tezos.transaction (action, 0tez, counter);
} with (list[op], store)
```

</Syntax>
<Syntax syntax="cameligo">

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

let dest = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" : address)

let proxy (action, store : parameter * storage) : return =
  let counter : parameter contract =
    match Tezos.get_contract_opt (dest) with
      Some contract -> contract
    | None -> failwith "Contract not found."
  in
  (* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. *)
  let mock_param = Increment (5n) in
  let op = Tezos.transaction action 0tez counter
  in [op], store
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
// counter.jsligo

type parameter =
| ["Increment", nat]
| ["Decrement", nat]
| ["Reset"];

// ...
```

```jsligo group=e
// proxy.jsligo

type parameter =
| ["Increment", nat]
| ["Decrement", nat]
| ["Reset"];

type storage = unit;

type return_ = [list<operation>, storage];

const dest = "KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" as address;

const proxy = (action: parameter, store: storage): return_ => {
  let counter =
    match (Tezos.get_contract_opt(dest), {
      Some: contract => contract,
      None: () => failwith("Contract not found.")
    });
  /* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. */
  let mock_param = Increment(5 as nat);
  let op = Tezos.transaction(action, 0 as tez, counter);
  return [list([op]), store];
};
```

</Syntax>
