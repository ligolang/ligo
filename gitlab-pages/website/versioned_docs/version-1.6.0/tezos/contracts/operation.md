---
id: operation
title: Operations
---

import Syntax from '@theme/Syntax';

The final stack after a contract execution is a pair containing a new
storage and a list of operations. An operation is either a transfer,
an account creation (a.k.a. origination), a delegation or the emission
of an event.

There are no literal values of type operation. Instead, such values
are created using the following functions from the standard library:
`Tezos.create_contract` (origination), `Tezos.transaction` (transfer),
`Tezos.set_delegate` (delegation), and `Tezos.emit` (emission of
event).

### Origination

<Syntax syntex="cameligo">

The call `Tezos.create_contract e d a s` returns a contract creation
operation (origination) for the entrypoint `e` (as a function) with
optional delegate `d`, initial amount `a` and initial storage `s`,
together with the address of the created contract. Note that the
created contract cannot be called immediately afterwards (that is,
`Tezos.get_contract_opt` on that address would return `None`), as the
origination must be performed successfully first, for example by
calling a proxy contract or itself.

```cameligo group=operation
type return = operation list * string

[@entry]
let main (_ : string) (storage : string) : return =
  let entrypoint (_ : nat) (storage : string) =
    (([] : operation list), storage) in
  let op, _addr : operation * address =
    Tezos.create_contract
      entrypoint
      (None : key_hash option)
      300000000mutez
      "one"
  in [op], storage
```

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.create_contract(e,d,a,s)` returns a contract creation
operation (origination) for the entrypoint `e` (as a function) with
optional delegate `d`, initial amount `a` and initial storage `s`,
together with the address of the created contract. Note that the
created contract cannot be called immediately afterwards (that is,
`Tezos.get_contract_opt` on that address would return `None()`), as the
origination must be performed successfully first, for example by
calling a proxy contract or itself.

```jsligo group=operation
type @return = [list<operation>, string];

@entry
const main = (_: string, storage: string) : @return => {
  const entrypoint = (_param: nat, storage: string) =>
    [list([]), storage];
  const [op, _addr]: [operation, address] =
    Tezos.create_contract(entrypoint,
                          (None() as option<key_hash>),
                          300000000mutez,
                          "one");
  return [[op], storage];
}
```

</Syntax>

### Transaction


<Syntax syntex="cameligo">

The call `Tezos.transaction param amount contract_addr` evaluates in
an operation that will send the amount `amount` in mutez to the
contract at the valid address `contract_addr`, with parameter
`param`. If the contract is an implicit account, the parameter must be
`unit`.

The following example shows a transaction sent from one contract to
another. The former is derived from a module `B` with an entrypoint
`increment`; the latter is derived from a module `A` with an
entrypoint `add`. The entrypoint `increment` calls `add` with `1`. We
assume that the contract associated with module `A` is deployed under
the address `"KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"`.

Contract (derived from) `B` needs to provide the parameter of the
called contract. The convention is to transform the entrypoint being
called to a variant. Here, `add` in `A` becomes `Add`. The variant
`Add` carries the value of the parameter of the entrypoint, here `1`
because we show how to increment the storage (`storage + delta` in
`A`). The type of the parameter of the contract `A` is obtained by `A
parameter_of`, and it is the type of `Add 1`.

```cameligo group=operation_transaction
type 'storage return = operation list * 'storage

module A =
  struct
    type storage = int

    [@entry]
    let add (delta : int) (storage : storage) : storage return =
      [], storage + delta
  end

module B =
  struct
    type storage = int

    [@entry]
    let increment (_param : unit) (storage : storage) : storage return =
      let contract_addr =
        Tezos.get_contract
          ("KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" : address) in
      let operation =
        Tezos.transaction (Add 1 : A parameter_of) 0tez contract_addr
    in [operation], storage
  end
```
</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.transaction(param, amount, contract_addr)` evaluates
in an operation that will send the amount `amount` in mutez to the
contract at the valid address `contract_addr`, with parameter
`param`. If the contract is an implicit account, the parameter must be
`unit`.

The following example shows a transaction sent from one contract to
another. The former is derived from a namespace `B` with an entrypoint
`increment`; the latter is derived from a namespace `A` with an
entrypoint `add`. The entrypoint `increment` calls `add` with `1`. We
assume that the contract associated with namespace `A` is deployed
under the address `"KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"`.

Contract (derived from) `B` needs to provide the parameter of the
called contract. The convention is to transform the entrypoint being
called to a variant. Here, `add` in `A` becomes `Add`. The variant
`Add` carries the value of the parameter of the entrypoint, here `1`
because we show how to increment the storage (`storage + delta` in
`A`). The type of the parameter of the contract `A` is obtained by `A
parameter_of`, and it is the type of `Add(1)`.

```jsligo group=operation_transaction
type @return<storage> = [list<operation>, storage];

namespace A {
  type storage = int;

  @entry
  const add = (delta: int, storage: storage): @return<storage> =>
    [[], storage + delta];
}

namespace B {
  type storage = int;

  @entry
  const increment = (_param: unit, storage: storage): @return<storage> => {
    const operation =
      Tezos.transaction(Add(1) as parameter_of A,
                        0tez,
                        Tezos.get_contract(
                          "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" as address))
    return [[operation], storage];
  }
}
```

</Syntax>

It is possible for a contract to have multiple entrypoints, which is
implicitly translated in LIGO to a `parameter` with a variant type as
shown below. The following contract:

<Syntax syntax="cameligo">

```cameligo group=entrypoints_and_annotations
type storage = int

[@entry]
let sub (i : int) (x : storage) : operation list * storage = [], x - i

[@entry]
let add (i : int) (x : storage) : operation list * storage = [], x + i
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=entrypoints_and_annotations
type storage = int

@entry
const sub = (i: int, x: storage) : [list<operation>, storage] =>
  [[], x - i]

@entry
const add = (i: int, x: storage) : [list<operation>, storage] =>
  [[], x + i]
```

</Syntax>

is translated internally to a contract similar to this one:

<Syntax syntax="cameligo">

```cameligo
type storage = int

type parameter = Sub of int | Add of int

[@entry]
let main (p : parameter) (x : storage) : operation list * storage =
  [],
  (match p with
  | Sub i -> x - i
  | Add i -> x + i)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
type storage = int;

type parameter = ["Sub", int] | ["Add", int];

let main = (p: parameter, x: storage): [list<operation>, storage] =>
  [list ([]),
  match(p) {
    when(Sub(i)): x - i;
    when(Add(i)): x + i
  }];
```

</Syntax>

This contract can be called by another contract, like this one:

<Syntax syntax="cameligo">

```cameligo group=get_entrypoint
type storage = int
type parameter = int

type remote_param = Sub of int

[@entry]
let main (_ : parameter) (s : storage): operation list * storage =
  let contract_addr =
    Tezos.get_entrypoint
      "%sub" // Corresponds to the `Sub` variant of `remote_param`.
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
  in [Tezos.transaction (Sub 2) 2mutez contract_addr], s
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=get_entrypoint
type storage = int;
type parameter = int;

type remote_param = | ["Sub", int]; // Note the leading vertical bar.

@entry
const main = (_p: parameter, s: storage): [list<operation>, storage] => {
  let contract_addr =
    Tezos.get_entrypoint(
      "%sub", // Corresponds to the `Sub` variant of `remote_param`.
      "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address);
  return [[Tezos.transaction(Sub(2), 2mutez, contract_addr)], s];
};
```

</Syntax>

Notice how we directly use the `%sub` entrypoint without mentioning
the `%add` entrypoint. This is done with the help of
annotations. Those annotations correspond to the name of the function
defining the entrypoints, prefixed by a `%`, so `sub` becomes
`%sub`. Also, as shown above, each entrypoint corresponds to an
implicit variant type whose name is the entrypoint function whose
first character is capitalised, so `sub` becomes `Sub`, as far as
parameters are concerned.

### Delegation

<Syntax syntax="cameligo">

The call `Tezos.set_delegate d` evaluates in an operation that sets
the delegate of the current smart contract to be `d`, where `d` is an
optional key hash. If `None`, the delegation is withdrawn. If the
contract has no delegation, then no change occurs. If `d` is `Some
kh`, where `kh` is the key hash of a registered delegate that is not
the current delegate of the contract, then this operation sets the
delegate of the contract to this registered delegate. A failure occurs
if `kh` is the current delegate of the contract or if `kh` is not a
registered delegate. However, the instruction in itself does not fail;
it produces an operation that will fail when applied.

```cameligo group=set_delegate
let check (kh : key_hash) : operation list =
  [Tezos.set_delegate (Some kh)]
```

</Syntax>


<Syntax syntax="jsligo">

The call `Tezos.set_delegate(d)` evaluates in an operation that sets
the delegate of the current smart contract to be `d`, where `d` is an
optional key hash. If `None()`, the delegation is withdrawn. If the
contract has no delegation, then no change occurs. If `d` is
`Some(kh)`, where `kh` is the key hash of a registered delegate that
is not the current delegate of the contract, then this operation sets
the delegate of the contract to this registered delegate. A failure
occurs if `kh` is the current delegate of the contract or if `kh` is
not a registered delegate. However, the instruction in itself does not
fail; it produces an operation that will fail when applied.

```jsligo group=set_delegate
const check = (kh: key_hash) : list<operation> =>
  [Tezos.set_delegate (Some(kh))];
```

</Syntax>


### Event emission

<Syntax syntax="cameligo">

The call `Tezos.emit event_tag event_type` evaluates in an operation
that will write an event into the transaction receipt after the
successful execution of this contract. The event is annotated by the
string `event_tag` if it is not empty. The argument `event_type` is
used only to specify the type of data attachment.

```cameligo group=event_emit
type storage = unit

[@entry]
let main (param : int * int) () : operation list * storage =
  [Tezos.emit "%foo" param; Tezos.emit "%bar" param.0], ()
```

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.emit(event_tag, event_type)` evaluates in an operation
that will write an event into the transaction receipt after the
successful execution of this contract. The event is annotated by the
string `event_tag` if it is not empty. The argument `event_type` is
used only to specify the type of data attachment.

```jsligo group=event_emit
type storage = unit;

@entry
const main = (param: [int, int], storage: unit) : [list<operation>, storage] =>
  [[Tezos.emit("%foo", param), Tezos.emit("%bar", param[0])], storage];
```


</Syntax>
