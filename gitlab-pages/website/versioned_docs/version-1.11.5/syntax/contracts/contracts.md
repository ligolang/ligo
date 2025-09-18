---
id: contracts
title: Contracts
---

import Syntax from '@theme/Syntax';

Smart contracts are programs that run on a blockchain.
For an overview of how smart contracts work on Tezos, see [An introduction to smart contracts](https://docs.tezos.com/smart-contracts) on docs.tezos.com.

For the data type that represents a contract, see [Contracts](../../data-types/contracts-type).

## Example contract

This example contract stores an integer and provides two entrypoints that allow callers to add to that integer or subtract from that integer.
The code includes automated tests for the contract that are not part of the contract itself; for more information about tests, see [Testing](../../testing).

<Syntax syntax="cameligo">

```cameligo group=starter_counter
module Test = Test.Next

module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let add (value : int) ( store: storage_type) : return_type =
    [], store + value

  [@entry]
  let sub (value : int) ( store: storage_type) : return_type =
    [], store - value

end

let test =

  let contract = Test.Originate.contract (contract_of Counter) 0 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "add" contract.taddr) 5 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "sub" contract.taddr) 2 0tez in
  Assert.assert ((Test.Typed_address.get_storage contract.taddr) = 3)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=starter_counter
import Test = Test.Next;

namespace Counter {
  type storage_type = int;
  type return_type = [list<operation>, storage_type];

  @entry
  const add = (value: int, store: storage_type): return_type =>
    [[], store + value];

  @entry
  const sub = (value: int, store: storage_type): return_type =>
    [[], store - value];

}

const test = (() => {

  const contract = Test.Originate.contract(contract_of(Counter), 0, 0tez);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("add", contract.taddr), 5, 0tez);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("sub", contract.taddr), 2, 0tez);
  Assert.assert(Test.Typed_address.get_storage(contract.taddr) == 3);

})();
```

</Syntax>

For more examples of contracts, see [Quickstart](../../tutorials/getting-started) or load one of the templates in the [Online IDE](https://ide.ligolang.org/).

## Components of a contract

A smart contract has three main elements:

- Its address: a contract is a kind of account, and can receive and send tez
- Its storage: data that is dedicated to and can be read and written only by the contract
- Its code: one or more entrypoints, which are a kind of function that can be called either from outside the chain or from other contracts

### Account and address

Originated (deployed) smart contracts are assigned an account address.
Just like user accounts, the smart contract can accept tez sent to its account and store it.
Its code can send the tez to other accounts.
Contracts can also delegate their tez to other accounts just like user accounts can.

In most cases, contracts have addresses that start with `KT1`.
However, they can also be tied to an implicit account and have an address that starts with `kt1`, `kt2` or `kt3`, [depending on the hashing function](https://tezos.gitlab.io/michelson-reference/#type-address).

#### Getting a contract's address

The function `Tezos.get_self_address` returns the address of the currently running contract.

This function can be misleading because it returns the address of the contract *executing the code*, which in rare cases can be different from the address of the contract in which the code is written.
For example, it's possible for one contract to run a lambda that is contained in another contract; in this case, the function returns the address of the contract that is running the lambda, not the contract that contains the lambda.

<Syntax syntax="cameligo">

```cameligo group=address
let current_addr : address = Tezos.get_self_address ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=address
const current_addr: address = Tezos.get_self_address();
```

</Syntax>

To get the address of the contract that contains the currently running code, use the function `Tezos.self(entrypoint)`.
This function returns the address of the contract that contains the code even if another contract is currently executing that code.

The string `entrypoint` is the name of a valid entrypoint such that `entrypoint` is not `"%default"`, or the empty string denoting the `"%default"` entrypoint (which is the root of the smart contract parameter if no `"%default"` entrypoint is explicitly defined).
If the contract does not have the specified entrypoint, the call results in an type checking error.

Naming convention: if you are using entrypoints, use `"%bar"` to
denote the constructor `"Bar"` of the parameter, in turn corresponding
to the entrypoint function `bar`. If you are not using entrypoints:
use `"%default"`.

<Syntax syntax="cameligo">

```cameligo group=self
let check () = Tezos.self("%default")
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=self
const check = () => Tezos.self("%default");
```

</Syntax>

### Storage

Smart contracts have persistent storage.
The originator of the contract sets the initial value of the storage.
After it is originated, only the contract itself can change the storage.

Contract storage is publicly readable by users outside the chain.
However, a contract can see only its own storage, not the storage of any other contracts.

#### Metadata

Contracts often store metadata that provides descriptive information about them to wallets, explorers, dApps, and other off-chain applications.
Contract metadata is stored off-chain and therefore on-chain applications including smart contracts cannot access it.
To store data off-chain in a decentralized way, many Tezos developers use [IPFS](https://ipfs.tech/).

The primary Tezos standard for metadata is
[TZIP-016 (Tezos Metadata Standard)](https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-16/tzip-16.md).

For more information about metadata, see [Adding package metadata](../../advanced/package-management#adding-package-metadata-ligo-manifest).

### Code

The code of a contract is publicly viewable and cannot be changed after the contract is originated (deployed).
A contract's code can include these elements:

- [Entrypoints](./entrypoints), which are the ways that the contract can be called, similar to a method or function in many programming languages or an endpoint in an API.

- [Views](./views), which are a way for contracts to expose information to other contracts and to off-chain consumers.

- Internal functions, which allow you to organize and reuse code.

<Syntax syntax="cameligo">

For clarity, LIGO developers often store the code of a contract in a single module.
However, LIGO does not require that contract code is in a module, that a module contains a contract, or that only one contract is in a module.

</Syntax>

<Syntax syntax="jsligo">

For clarity, LIGO developers often store the code of a contract in a single namespace.
However, LIGO does not require that contract code is in a namespace, that a namespace contains a contract, or that only one contract is in a namespace.

</Syntax>
