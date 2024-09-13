---
id: contracts-type
title: Contracts
---

import Syntax from '@theme/Syntax';

The `contract` type represents a smart contract.
There is no way to create a literal value of this type; you must create a `contract` type by passing the address of the account to a predefined function.
Beware of failures if the address is invalid.

<Syntax syntax="cameligo">

The call `Tezos.implicit_account kh` casts the public key hash `kh` of an implicit account to a `contract` type that represents that user account.
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
  let myContract: contractParam contract = Tezos.get_contract contractAddress in
  let operation = Tezos.transaction (Increment 4) 0tez myContract in
  [operation], storage
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

@entry
const callContract = (_: unit, storage: int): returnType => {
  const contractAddress: address = ("KT1FpuaoBHwXMXJ6zn3F4ZhpjpPZV28MAinz" as address);
  const myContract: contract<contractParam> = Tezos.get_contract(contractAddress);
  const contractArg: contractParam = Increment(4);
  const operation = Tezos.transaction(contractArg, 0tez, myContract);
  return [list([operation]), storage + 1]
}
```

</Syntax>