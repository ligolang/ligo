---
id: entrypoints
title: Entrypoints
---

import Syntax from '@theme/Syntax';

The entrypoints of a contract represent the different ways that it can be called, similar to a method or function in many programming languages or an endpoint of an API.
A contract can have any number of internal functions, but only the functions designated as entrypoints can be called by outside consumers and other contracts.

Contracts must have at least one entrypoint, and they can have as many as needed.
For example, the following contract exports two entrypoints, named `increment` and `decrement`.

<Syntax syntax="jsligo">

```jsligo group=incdec
export namespace IncDec {
  type storage = int;
  type result = [list<operation>, storage];

  // Four entrypoints

  @entry
  const increment = (delta : int, store : storage) : result =>
    [[], store + delta];

  @entry
  const @default = (_u : unit, store : storage) : result =>
    increment(1, store)

  @entry
  const decrement = (delta : int, store : storage) : result =>
    [[], store - delta];

  @entry
  const reset = (_p : unit, _s : storage) : result =>
    [[], 0];
};
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo group=incdec
module IncDec = struct
  type storage = int
  type result = operation list * storage

  (* Four entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : result =
    [], store + delta
  [@entry] let default (() : unit) (store : storage) : result =
    increment 1 store
  [@entry] let decrement (delta : int) (store : storage) : result =
    [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : result =
    [], 0
end
```

</Syntax>

To call an entrypoint, pass the name of the entrypoint with an initial capital and the parameter.
For example, this `run dry-run` command calls the `increment` entrypoint in the previous contract:

<Syntax syntax="cameligo">

```bash
ligo run dry-run -m IncDec gitlab-pages/docs/tezos/contracts/src/entrypoints/incdec.mligo 'Increment(5)' '4'
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo run dry-run -m IncDec gitlab-pages/docs/tezos/contracts/src/entrypoints/incdec.jsligo 'Increment(5)' '4'
```

</Syntax>

The response shows an empty list of transactions to run next and the new state of the storage:

```
( LIST_EMPTY() , 9 )
```

Note that even though the entrypoint name starts with a lower-case letter, the command uses an initial upper-case letter to call it.

## Parameters

LIGO entrypoints always receive two parameters:

- A parameter passed by the caller
- The current value of the contract storage

The caller provides only the first parameter; the LIGO framework provides the current value of the contract storage.

The caller-provided parameter can be of any type, including:

- Unit, to indicate no information
- A primitive data type such as integer or string
- A complex data type such as a tuple or list

Although technically speaking the entrypoint receives only one parameter from the caller, it can behave as though it receives multiple parameters by setting a complex type as the parameter type and destructuring the parameter into multiple variables.
For example, this entrypoint accepts a parameter that consists of an integer, a string, and a Boolean:

<Syntax syntax="cameligo">

```cameligo group=complex_param
type complexParam = int * string * bool

type storage = int * string
type return_type = operation list * storage

[@entry] let dosomething (param : complexParam) (storage : storage) : return_type =
  let (intParam, stringParam, boolParam) = param in
  if boolParam then
    [], (intParam, stringParam)
  else
    [], storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=complex_param
type complexParam = [
  int,
  string,
  bool,
];

type storage = [int, string];
type return_type = [list<operation>, storage];

@entry
const dosomething = (param: complexParam, storage: storage): return_type => {
  const [intParam, stringParam, boolParam] = param;
  if (boolParam) {
    return [[], [intParam, stringParam]];
  }
  return [[], storage];
}
```

</Syntax>

## Return values

LIGO entrypoints must return a tuple that contains these values:

- A list of operations to run after the entrypoint completes, such as calls to other smart contracts or transfers of tez to accounts
- The new value of the contract storage, even if the entrypoint did not change it

Unlike functions and API endpoints, entrypoints do not return a value directly to the caller.
To return data from a smart contract, you can use one of these methods:

- Use views to return data to smart contracts or off-chain applications
- Use events to return data to off-chain applications
- Include a callback parameter that sends information to another smart contract by calling one of its entrypoints

## Logic

An entrypoint may run logic based on:

- The contract storage
- The parameters that senders pass
- Transaction context values such as `Tezos.get_amount` and `Tezos.get_sender`
- The table of constants

Entrypoints cannot access information outside of Tezos, such as calling external
APIs.
If an entrypoint needs information from outside Tezos it must use oracles;
see [Oracles](https://docs.tezos.com/smart-contracts/oracles) on docs.tezos.com
and [Using and trusting Oracles](https://opentezos.com/smart-contracts/oracles/)
on opentezos.com.

## Storing and sending tez

Smart contracts are a type of account and can store and send tez.
By default, contracts accept any tez sent to them.

If you don't want an entrypoint to accept tez, check how much tez was included with the transaction and fail the transaction if it is more than zero, as in this example:

<Syntax syntax="cameligo">

```cameligo group=c
type parameter = unit
type storage = unit
type result = operation list * storage

[@entry]
let no_tokens (action : parameter) (storage : storage) : result =
  if Tezos.get_amount () > 0tez then
    failwith "This contract does not accept tokens."
  else ([], storage)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
type parameter = unit;
type storage = unit;
type result = [list<operation>, storage];

@entry
const no_tokens = (action: parameter, storage: storage): result => {
  if (Tezos.get_amount() > 0tez) {
    return failwith("This contract does not accept tokens.");
  } else {
    return [[], storage];
  };
};
```

</Syntax>

To send tez, create a transaction with `Tezos.transaction` and return it in the list of operations at the end of the entrypoint, as in this example:

<Syntax syntax="cameligo">

```cameligo group=send_tez
type storage = unit
type return_value = operation list * storage

[@entry] let give5tez (_ : unit) (storage : storage) : return_value =
  if Tezos.get_balance () >= 5tez then
    let receiver_contract = match Tezos.get_contract_opt (Tezos.get_sender ()) with
      Some contract -> contract
    | None -> failwith "Couldn't find account" in
    let operation = Tezos.Next.Operation.transaction unit 5tez receiver_contract in
    [operation], storage
  else
    [], storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=send_tez
type storage = unit;
type return_value = [list<operation>, storage];

@entry
const give5tez = (_: unit, storage: storage): return_value => {
  let operations: list<operation> = [];
  if (Tezos.get_balance() >= 5tez) {
    const receiver_contract = match(Tezos.get_contract_opt(Tezos.get_sender())) {
      when(Some(contract)): contract;
      when(None): failwith("Couldn't find account");
    };
    operations = [Tezos.Next.Operation.transaction(unit, 5tez, receiver_contract)];
  }
  return [operations, storage];
}
```

</Syntax>

## Access control

This example shows how `Tezos.get_sender` can be used to deny access to an
entrypoint:

<Syntax syntax="cameligo">

```cameligo group=c
let owner = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)

[@entry]
let owner_only (action : parameter) (storage: storage) : result =
  if Tezos.get_sender () <> owner then failwith "Access denied."
  else ([], storage)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
const owner: address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

@entry
const owner_only = (action: parameter, storage: storage): result => {
  if (Tezos.get_sender() != owner) { return failwith("Access denied."); }
  else { return [[], storage]; };
};
```

</Syntax>

:::note

The entrypoint in the previous example uses `Tezos.get_sender` instead of `Tezos.get_source` to prevent a security flaw.
For more information, see the [Security tutorial](../../tutorials/security/security.md#incorrect-authorisation-checks).

:::

## Calling other contracts

An entrypoint can create any number of calls to other entrypoints in its contract and to other contracts.
However, as described in [Operations](https://docs.tezos.com/smart-contracts/logic/operations) on docs.tezos.com, these calls happen after the entrypoint code has completed.

To call other entrypoints or contracts, create an operation and return it in the list of operations at the end of the entrypoint, as in this example:

<Syntax syntax="cameligo">

```cameligo group=call_entrypoint
type storage = unit
type return_value = operation list * storage

[@entry] let callContract (param : (address * string)) (storage : storage): return_value =
  let (addr, parameter) = param in
  let receiver_contract = match Tezos.get_contract_opt(addr) with
    Some contract -> contract
  | None -> failwith "Couldn't find contract" in
  let operations = [Tezos.Next.Operation.transaction parameter 0tez receiver_contract] in
  operations, storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=call_entrypoint
type storage = unit;
type return_value = [list<operation>, storage];

@entry
const callContract = (param: [address, string], storage: storage): return_value => {
  const [addr, parameter] = param;
  const receiver_contract = match(Tezos.get_contract_opt(addr)) {
    when(Some(contract)): contract;
    when(None): failwith("Couldn't find contract");
  }
  const operations = [Tezos.Next.Operation.transaction(parameter, 0tez, receiver_contract)];
  return [operations, storage];
}
```

</Syntax>

It's important to remember that the transactions an entrypoint creates do not run until after the entrypoint code has completed.
The entrypoint cannot take advantage of any changes that these transactions make.
For example, if an entrypoint creates a transaction that sends tez to another contract and then checks its balance again before the end of the entrypoint, the balance is the same as it was at the start of the entrypoint.
Its balance changes only when it returns the transaction at the end of the entrypoint code and that transaction runs.

## The default entrypoint

The name `default` has a special meaning for a Tezos entrypoint.
It denotes the default entrypoint that is called unless another is specified.
Because `default` is a reserved keyword in JsLIGO, if you want to create an entrypoint named `default`, you must escape its name as `@default`.

For more information about the default entrypoint and its internal behavior, see [Implementation details: the default entrypoint](https://docs.tezos.com/smart-contracts/entrypoints#implementation-details-the-default-entrypoint) on docs.tezos.com.

## The main function

In earlier versions of LIGO, it was possible to write a contract that had a single main function named `main` that branched according to the parameter passed to it.
This way, contracts could behave as though they had multiple entrypoints while having only a single function.
While it is still possible to define a single function called `main` and mark it as the sole entry point using `@entry`, this is not what most contracts should do.

**This feature is deprecated. Future versions of LIGO will not allow the declaration of a single `main` function. A workaround is given at the end of this section.**

A common way to code a contract using a single main function is to use a variant type as its parameter and branch the code according to the parameter.
In the following example, the storage contains a counter of type `nat` and a name of type `string`.
Depending on the parameter of the contract, either the counter or the name is updated.

<Syntax syntax="cameligo">

```cameligo group=contract_main
type parameter =
  Action_A of nat
| Action_B of string

type storage = {
  counter : nat;
  name    : string
}

type result = operation list * storage

let entry_A (n : nat) (store : storage) : result =
  [], {store with counter = n}

let entry_B (s : string) (store : storage) : result =
  [], {store with name = s}

[@entry]
let main (action : parameter) (store: storage) : result =
  match action with
    Action_A n -> entry_A n store
  | Action_B s -> entry_B s store
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=contract_main
export type parameter =
| ["Action_A", nat]
| ["Action_B", string];

export type storage = {
  counter : nat,
  name    : string
};

type result = [list<operation>, storage];

const entry_A = (n: nat, store: storage): result =>
  [[], {...store, counter: n}];

const entry_B = (s: string, store: storage): result =>
  [[], {...store, name: s}];

@entry
const main = (action: parameter, store: storage): result =>
  match(action) {
    when(Action_A(n)): entry_A(n, store);
    when(Action_B(s)): entry_B(s, store)
  };
```

</Syntax>

To call a contract that has a single main function instead of separate entrypoints, pass the parameter value without an entrypoint name.
For example, this `run dry-run` command passes the `Action_A` variant to the contract in the previous example:

<Syntax syntax="cameligo">

```bash
ligo run dry-run gitlab-pages/docs/tezos/contracts/src/entrypoints/contract_main.mligo 'Action_A(5n)' '(5n, "hello")'
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo run dry-run gitlab-pages/docs/tezos/contracts/src/entrypoints/contract_main.jsligo 'Action_A(5n)' '[5n, "hello"]'
```

</Syntax>

### Workaround for the deprecation of the `main` function

In most cases, adding `[@entry]` for CameLIGO or `@entry` for JsLIGO
before the existing `main` function should suffice. However in cases
where it is not possible or desirable to convert an existing
`contract_main` contract to the new `@entry` format (e.g. generated
code or a code review process that forbids making changes to an
already-audited file), the deprecation can be circumvented by adding a
proxy file which declares a single entry point and calls the existing
`main` function, as follows:

<Syntax syntax="cameligo">

```cameligo group=contract_main_proxy
#import "gitlab-pages/docs/tezos/contracts/src/entrypoints/contract_main.mligo" "C"

module Proxy = struct

  [@entry]
  let proxy (p : C.parameter) (s : C.storage) : operation list * C.storage =
    C.main p s

end
```

The contract can then be compiled using the following command:

```shell
ligo compile contract --library . -m Proxy gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main_proxy.mligo
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=contract_main_proxy
#import "gitlab-pages/docs/tezos/contracts/src/entrypoints/contract_main.jsligo" "C"

namespace Proxy {
  @entry
  const proxy =
    (p: C.parameter, s: C.storage): [list<operation>, C.storage] =>
    C.main(p, s)
}
```

The contract can then be compiled using the following command:

```shell
ligo compile contract --library . \
  -m Proxy \
  gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main_proxy.jsligo
```

</Syntax>

Notice that to compile a parameter for this contract, now we need to
pass the either `-e proxy` or construct a value using the `Proxy`
constructor:

<Syntax syntax="cameligo">

```shell
ligo compile parameter --library . \
  -m Proxy -e proxy \
  gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main_proxy.mligo \
  "Action_A(42n)"
```

```shell
ligo compile parameter --library . \
  -m Proxy \
  gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main_proxy.mligo \
  "Proxy(Action_A(42n))"
```

</Syntax>

<Syntax syntax="jsligo">

```shell
ligo compile parameter --library . \
  -m Proxy -e proxy \
  gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main_proxy.jsligo \
  "Action_A(42n)"
```

```shell
ligo compile parameter --library . \
  -m Proxy \
  gitlab-pages/docs/advanced/src/entrypoints-contracts/contract_main_proxy.jsligo \
  "Proxy(Action_A(42n))"
```

</Syntax>

<!-- updated use of entry -->
