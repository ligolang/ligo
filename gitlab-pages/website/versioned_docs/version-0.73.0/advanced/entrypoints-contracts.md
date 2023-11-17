---
id: entrypoints-contracts
title: Main function and Entrypoints
---

import Syntax from '@theme/Syntax';

## Entry points

A LIGO smart contract is made of a series of constant and function
declarations. Only functions having a special type can be called when
the contract is activated: we call them *entry points*. An entry point
function takes two parameters, the *contract parameter* and the
*on-chain storage*, and returns a pair made of a *list of operations*
and a (new) storage value.

A smart contract can export more than one entry point function.
An entry point can be selected by specifying its name when calling the contract.
For example, the following contract exports two functions, named `increment` and `decrement`.
The `increment` function can be called by passing `Increment (10)` to the contract (notice the capitalization of `Increment`).
More examples on how to perform this call are given below.

<Syntax syntax="jsligo">

```jsligo skip
namespace IncDec {
  type storage = int;
  type ret = [list<operation>, storage];

  // Four entrypoints

  @entry
  const increment = (delta : int, store : storage) : ret =>
    [list([]), store + delta];

  @entry
  const @default = (_u : unit, store : storage) : result =>
    increment(1, store)

  @entry
  const decrement = (delta : int, store : storage) : ret =>
    [list([]), store - delta];

  @entry
  const reset = (_ : unit, _ : storage) : ret =>
    [list([]), 0];
};
```

</Syntax>

<Syntax syntax="jsligo">

Note that the name `default` has a special meaning for a Tezos entry point,
and denotes the default entry point to be called unless another one is
specified. Due to the fact that `default` is a reserved keyword in JsLIGO,
we use the escape notation `@default` to write the function name, without
it being misinterpreted as a keyword.

</Syntax>

<Syntax syntax="cameligo">

```cameligo skip
module IncDec = struct
  type storage = int
  type return = operation list * storage

  (* Four entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : return =
    [], store + delta
  [@entry] let default (() : unit) (store : storage) : result =
    increment 1 store
  [@entry] let decrement (delta : int) (store : storage) : return =
    [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : return =
    [], 0
end
```

</Syntax>

When the contract is originated, the initial value of the storage is
provided. When an entry point is later called, only the parameter is
provided by the user, and the blockchain (or testing framework)
supplies the current storage value as a second argument.

The type of the contract parameter and the storage are up to the
contract designer, but the type for the list of operations is not.
The return type of an entry point is as follows, assuming that the type
`storage` has been defined elsewhere. (Note that you can use any type
with any name for the storage.)

<Syntax syntax="jsligo">

Note that the name `default` has a special meaning for a Tezos entry point,
and denotes the default entry point to be called unless another one is
specified. Due to the fact that `default` is a reserved keyword in LIGO,
we use the escape notation `@default` to write the function name, without
it being misinterpreted as a keyword.

</Syntax>

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

The contract storage can only be modified by activating an entry point:
given the state of the storage *on-chain*, an entry point function
specifies how to create another state for it, depending on the
contract's parameter.

## Calling a contract

### Using the dry-run command

In order to call the `increment` entry point of the smart contract, we can pass the `-m IncDec` option to specify the module and the `-e increment` option to specify the entry point.

<Syntax syntax="cameligo">

```bash
ligo run dry-run -m IncDec -e increment incdec.jsligo '5' '0' 
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo run dry-run -m IncDec -e increment incdec.jsligo '5' '0' 
```

</Syntax>

In the command above, `0` is the initial `storage`, and `5` is the `delta` argument.

### Calling an on-chain contract

When a contract is deployed on-chain, the Michelson value for the parameter can be obtained with:

<Syntax syntax="cameligo">

```bash
ligo compile parameter -m IncDec incdec.mligo 'Increment(5)'
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo compile parameter -m IncDec incdec.jsligo 'Increment(5)'
```

</Syntax>

In the command above, `Increment` is the (capitalized) name of the entry point to call, and `5` is the `delta` argument.

### Using the WebIDE

<Syntax syntax="cameligo">

![Clicking on Dry Run in the WebIDE](img-entrypoint-contracts/webide-mligo.png)

</Syntax>

<Syntax syntax="jsligo">

![Clicking on Dry Run in the WebIDE](img-entrypoint-contracts/webide-jsligo.png)

</Syntax>

### Using the `ligo run test` command

A LIGO program can instantiate a new contract (or obtain an existing contract from its address),
and call one of its entry points by passing e.g. the parameter `Increment(5)`.

<Syntax syntax="cameligo">

```jsligo skip
#import "incdec.mligo" "C"

let test =
  let (ta, _, _) = Test.originate_module (contract_of C.IncDec) 0 (0tez) in
  let c : C.IncDec parameter_of contract = Test.to_contract ta in
  let _ = Test.transfer_to_contract_exn c (Increment 42) (0tez) in
  assert (42 = Test.get_storage(ta))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
#import "incdec.jsligo" "C"

const test = (() => {
  let [ta, _, _] = Test.originate_module(contract_of(C.IncDec), 0, 0 as tez);
  let c : contract<parameter_of C.IncDec> = Test.to_contract(ta);
  let _ = Test.transfer_to_contract_exn(c, Increment(42), 0 as tez);
  assert(42 == Test.get_storage(ta));
})();
```

</Syntax>

The file above can be run with e.g. the `ligo run test` sub-command.

<Syntax syntax="cameligo">

```bash
ligo run test test.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo run test test.jsligo
```

</Syntax>

## Main function

For more control over the contract's API, it is possible to declare
*one* main function called `main`, that dispatches the control flow
according to its parameter. When declaring *entrypoints* using the
`@entry` annotation, LIGO automatically generates a `main` function,
but it is possible to write such a function by hand instead of using
the `@entry` facility.

As an analogy, in the C programming language, the `main` function is
the unique main function and any function called from it would be an
entrypoint.

Usually, the parameter of the contract is then a variant type, and, depending
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

It is possible to obtain the behaviour of normal function "calls" using
*views*, which are pure functions that do not modify the callee's on-chain
state. However, this section describes inter-contract invocations which
are queued, and may modify the callee's state.

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
  Increment of int
| Decrement of int
| Reset

type storage is unit

type return is list (operation) * storage
```

```pascaligo group=d
// proxy.ligo

type parameter is
  Increment of int
| Decrement of int
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

  const _mock_param : parameter = Increment (5);
  const op = Tezos.transaction (action, 0tez, counter);
} with (list[op], store)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
// counter.mligo

type parameter =
  Increment of int
| Decrement of int
| Reset

// ...
```

```cameligo group=d
// proxy.mligo

type parameter =
  Increment of int
| Decrement of int
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
  let mock_param = Increment 5 in
  let op = Tezos.transaction action 0tez counter
  in [op], store
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
// counter.jsligo

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

// ...
```

```jsligo group=e
// proxy.jsligo

type parameter =
| ["Increment", int]
| ["Decrement", int]
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
  let mock_param = Increment(5);
  let op = Tezos.transaction(action, 0 as tez, counter);
  return [list([op]), store];
};
```

</Syntax>
