---
id: entrypoints-contracts
title: Entrypoints to Contracts
---

## Entrypoints

A LIGO contract is made of a series of constant and function
declarations. Only functions having a special type can be called when
the contract is activated: they are called *entrypoints*. An
entrypoint need to take two parameters, the *contract parameter* and
the *on-chain storage*, and return a pair made of a *list of
operations* and a (new) storage.

When the contract is originated, the initial value of the storage is
provided. When and entrypoint is later called, only the parameter is
provided, but the type of an entrypoint contains both.

The type of the contract parameter and the storage are up to the
contract designer, but the type for list operations is not. The return
type of an entrypoint is as follows, assuming that the type `storage`
has been defined elsewhere. (Note that you can use any type with any
name for the storage.)

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
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

The contract storage can only be modified by activating an
entrypoint. It is important to understand what that means. What it
does *not* mean is that some global variable holding the storage is
modified by the entrypoint. Instead, what it *does* mean is that,
given the state of the storage *on-chain*, an entrypoint specifies how
to create another state for it, depending on a parameter.

Here is an example where the storage is a single natural number that
is updated by the parameter.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

```pascaligo group=a
type storage is nat
type return is list (operation) * storage

function save (const parameter : nat; const store : storage) : return is
  ((nil : list (operation)), parameter)
```

<!--CameLIGO-->
```cameligo group=a
type storage = nat

let save (parameter, store: nat * storage) : return =
  (([] : operation list), parameter)
```

<!--ReasonLIGO-->
```reasonligo group=a
type storage = nat;

let main = ((parameter, store): (nat, storage)) : return => {
  (([] : list (operation)), parameter);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

In LIGO, the design pattern for entrypoints consists in actually
having exactly *one entrypoint*, like the `main` function in C. The
parameter of the contract is then a variant type, and, depending on
the constructors of that type, different functions in the contract are
called. In other terms, the unique entrypoint dispatches the control
flow depending on a *pattern matching* on the contract parameter.

In the following example, the storage contains a counter (of type
`nat`) and a name (of type `string`). Depending on the parameter of
the contract, either the counter or the name is updated.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->
```pascaligo group=b
type parameter is
  Entrypoint_A of nat
| Entrypoint_B of string

type storage is record [
  counter : nat;
  name    : string
]

type return is list (operation) * storage

function handle_A (const n : nat; const store : storage) : return is
  ((nil : list (operation)), store with record [counter = n])

function handle_B (const s : string; const store : storage) : return is
  ((nil : list (operation)), store with record [name = s])

function main (const param : parameter; const store : storage): return is
  case param of
    Entrypoint_A (n) -> handle_A (n, store)
  | Entrypoint_B (s) -> handle_B (s, store)
  end
```

<!--CameLIGO-->
```cameligo group=b
type parameter =
  Entrypoint_A of nat
| Entrypoint_B of string

type storage = {
  counter : nat;
  name    : string
}

type return = operation list * storage

let handle_A (n, store : nat * storage) : return =
  ([] : operation list), {store with counter = n}

let handle_B (s, store : string * storage) : return =
  ([] : operation list), {store with name = s}

let main (param, store: parameter * storage) : return =
  match param with
    Entrypoint_A n -> handle_A (n, store)
  | Entrypoint_B s -> handle_B (s, store)
```

<!--ReasonLIGO-->
```reasonligo group=b
type parameter =
| Entrypoint_A (nat)
| Entrypoint_B (string);

type storage = {
  counter : nat,
  name    : string
};

type return = (list (operation), storage);

let handle_A = ((n, store): (nat, storage)) : return => {
  (([] : list (operation)), {...store, counter : n}); };

let handle_B = ((s, store): (string, storage)) : return => {
  (([] : list (operation)), {...store, name : s}); };

let main = ((param, store): (parameter, storage)) : return => {
  switch (param) {
  | Entrypoint_A (n) => handle_A ((n, store))
  | Entrypoint_B (s) => handle_B ((s, store))
  }
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
<!--Pascaligo-->
```pascaligo group=c
type parameter is unit
type storage is unit
type return is list (operation) * storage

function deny (const param : parameter; const store : storage) : return is
  if amount > 0mutez then
    (failwith ("This contract does not accept tokens.") : return)
  else ((nil : list (operation)), store)
```

<!--CameLIGO-->
```cameligo group=c
type parameter = unit
type storage = unit
type return = operation list * storage

let deny (param, store : parameter * storage) : return =
  if amount > 0mutez then
    (failwith "This contract does not accept tokens.": return)
  else (([] : operation list), store)
```

<!--ReasonLIGO-->
```reasonligo group=c
type parameter = unit;
type storage = unit;
type return = (list (operation), storage);

let deny = ((param, store): (parameter, storage)) : return => {
  if (amount > 0mutez) {
    (failwith("This contract does not accept tokens."): return); }
  else { (([] : list (operation)), store); };
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Access Control

This example shows how `sender` or `source` can be used to deny access to an entrypoint.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=c
const owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);

function filter (const param : parameter; const store : storage) : return is
  if source =/= owner then (failwith ("Access denied.") : return)
  else ((nil : list(operation)), store)
```

<!--CameLIGO-->
```cameligo group=c
let owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)

let filter (param, store: parameter * storage) : return =
  if source <> owner then (failwith "Access denied." : return)
  else (([] : operation list), store)
```

<!--ReasonLIGO-->
```reasonligo group=c
let owner : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);

let main = ((param, store): (parameter, storage)) : storage => {
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

In our case, we have a `counter.ligo` contract that accepts a
parameter of type `action`, and we have a `proxy.ligo` contract that
accepts the same parameter type, and forwards the call to the deployed
counter contract.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->
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

function proxy (const param : parameter; const store : storage): return is
  block {
    const counter : contract (parameter) = get_contract (dest);
    (* Reuse the parameter in the subsequent
       transaction or use another one, `mock_param`. *)
    const mock_param : parameter = Increment (5n);
    const op : operation = transaction (param, 0mutez, counter);
    const ops : list (operation) = list [op]
  } with (ops, store)
```

<!--CameLIGO-->
```cameligo skip
// counter.mligo

type paramater =
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

let proxy (param, store : parameter * storage) : return =
  let counter : parameter contract = Operation.get_contract dest in
  (* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. *)
  let mock_param : parameter = Increment (5n) in
  let op : operation = Operation.transaction param 0mutez counter
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

let proxy = ((param, store): (parameter, storage)) : return =>
  let counter : contract (parameter) = Operation.get_contract (dest);
  (* Reuse the parameter in the subsequent
     transaction or use another one, `mock_param`. *)
  let mock_param : parameter = Increment (5n);
  let op : operation = Operation.transaction (param, 0mutez, counter);
  ([op], store);
```

<!--END_DOCUSAURUS_CODE_TABS-->
