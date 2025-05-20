---
id: contracts-type
title: Contracts
---

import Syntax from '@theme/Syntax';

The `contract` type represents a smart contract.
There is no way to create a literal value of this type; you must create a `contract` type by passing the address of the account to a predefined function.
Beware of failures if the address is invalid.

For information about the components of a contract and how to use them, see [Contracts](../syntax/contracts).

## Creating contract types in contracts

<Syntax syntax="cameligo">

The call `Tezos.implicit_account kh` casts the public key hash `kh` of an implicit (user) account to a `contract` type that represents that user account.
Contract types that represent implicit accounts always have the type `unit contract` because they accept no parameter.

The call `Tezos.get_contract address` casts the address of a smart contract (originated account) to a `contract` type that represents that contract.
The type is parameterized based on the parameter that the contract accepts.
For example, if the contract accepts an integer, the type is `int contract`.

```cameligo group=get_contract
type returnType = operation list * int

type contractParam =
Reset
| Decrement of int
| Increment of int

[@entry]
let callContract (_ : unit) (storage : int) : returnType =
  let contractAddress : address = ("KT1FpuaoBHwXMXJ6zn3F4ZhpjpPZV28MAinz" : address) in
  let myContract : contractParam contract =
    Tezos.get_contract contractAddress in
  let operation =
    Tezos.Operation.transaction (Increment 4) 0tez myContract
  in [operation], storage
```

</Syntax>

<Syntax syntax="jsligo">

The call `Tezos.implicit_account(kh)` casts the public key hash `kh` of an implicit account to a `contract` type that represents that user account.
Contract types that represent implicit accounts always have the type `contract<unit>` because they accept no parameter.

The call `Tezos.get_contract(address)` casts the address of a smart contract (originated account) to a `contract` type that represents that contract.
The type is parameterized based on the parameter that the contract accepts.
For example, if the contract accepts an integer, the type is `contract<int>`.

```jsligo group=get_contract
type returnType = [list<operation>, int];

type contractParam =
  | ["Reset", unit]
  | ["Decrement", int]
  | ["Increment", int];

// @entry
function callContract (_: unit, storage: int): returnType {
  const contractAddress: address = ("KT1FpuaoBHwXMXJ6zn3F4ZhpjpPZV28MAinz" as address);
  const myContract: contract<contractParam> = Tezos.get_contract(contractAddress);
  const contractArg: contractParam = ["Increment" as "Increment", 4];
  const operation =
    Tezos.Operation.transaction(contractArg, 0 as tez, myContract);
  return [[operation], storage + 1]
}
```

</Syntax>

## Creating contract types in tests

<Syntax syntax="cameligo">

To create a contract type in a test, use the `contract_of` function, which accepts a module and returns a contract type.
See [Testing](../testing).

</Syntax>

<Syntax syntax="jsligo">

To create a contract type in a test, use the `contract_of` function, which accepts a namespace and returns a contract type.
See [Testing](../testing).

</Syntax>

## Implicit types and functions

<Syntax syntax="cameligo">

When declaring the entry points of a contract using `@entry`, LIGO generates two hidden values in the module:

* An implicit `main` function, which can be obtained using the keyword `contract_of(C)` where `C` is the namespace or module containing the entry points
* The input type for that `main` function, which can be obtained using the keyword `parameter_of C`

In the example below, `contract_of(C)` returns the implicitly-declared `main` function that calls the `increment` or `decrement` entry points depending on the argument given, and `parameter_of C` is the [variant](./variants) `["Increment", int] | ["Decrement", int]`.

```cameligo group=contract_of
type storage = int
type return = operation list * storage

module C = struct
  [@entry]
  let decrement (param : int) (storage : storage) : return =
    [], storage   - param

  [@entry]
  let increment (param : int) (storage : storage) : return =
    [], storage + param

  [@entry]
  let reset () (_ : storage) : return = [], 0
end

let test_initial_storage () : unit =
  let init_storage = 42 in
  let fee = 0mutez in
  let contract = Test.Originate.contract (contract_of C) init_storage fee in

  (* Call contract through entrypoints *)
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "increment" contract.taddr) 15 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "decrement" contract.taddr) 14 0tez in

  (* Call contract through `main` function *)
  let increment_param : C parameter_of = Increment 8 in
  let decrement_param : C parameter_of = Decrement 3 in
  let _ = Test.Typed_address.transfer_exn contract.taddr increment_param 0mutez in
  let _ = Test.Typed_address.transfer_exn contract.taddr decrement_param 0mutez in

  let new_storage = Test.Typed_address.get_storage contract.taddr
  in Assert.assert (new_storage = init_storage + 15 - 14 + 8 - 3)
```

</Syntax>

<Syntax syntax="jsligo">

When declaring the entry points of a contract using `@entry`, LIGO generates two hidden values in the module:

* An implicit `main` function, which can be obtained using the keyword `contract_of(C)` where `C` is the namespace or module containing the entry points
* The input type for that `main` function, which can be obtained using the keyword `parameter_of(C)`

In the example below, `contract_of(C)` returns the implicitly-declared `main` function that calls the `increment` or `decrement` entry points depending on the argument given, and `parameter_of(C)` is the [variant](./variants) `["Increment", int] | ["Decrement", int]`.

```jsligo group=contract_of
type storage = int;
type return_ = [list<operation>, storage];

class C {
  @entry
  decrement = (param: int, storage: storage) : return_ =>
    [[], storage - param];

  @entry
  increment = (param: int, storage: storage) : return_ =>
    [[], storage + param];

  @entry
  reset = (_unit: unit, _storage: storage) : return_ =>
    [[], 0];
}

const test_initial_storage = () : unit => {
  const init_storage = 42;
  const fee = 0 as mutez;
  const contract = Test.Originate.contract(contract_of(C), init_storage, fee);

  // Call contract through entrypoints
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("increment", contract.taddr), 15, 0 as tez);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("decrement", contract.taddr), 14, 0 as tez);

  // Call contract through `main` function
  const increment_param: parameter_of<C> = ["Increment" as "Increment", 8];
  const decrement_param: parameter_of<C> = ["Decrement" as "Decrement", 3];
  Test.Typed_address.transfer_exn(contract.taddr, increment_param, 0 as mutez);
  Test.Typed_address.transfer_exn(contract.taddr, decrement_param, 0 as mutez);

  const new_storage = Test.Typed_address.get_storage(contract.taddr);
  Assert.assert(new_storage == init_storage + 15 - 14 + 8 - 3);
}
```

</Syntax>
