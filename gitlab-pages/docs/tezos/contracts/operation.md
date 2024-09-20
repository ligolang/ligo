---
id: operation
title: Operations
---

import Syntax from '@theme/Syntax';

The final stack after a Tezos contract execution is a pair containing a new storage and a list of operations.
Common types of operations that smart contracts create include:

- Transfers (calling a smart contract or transferring tez to an account)
- Deploying (originating) a smart contract
- Changing the delegation for the current smart contract
- Emitting an [event](../../contract/events)

For this reason, LIGO entrypoints always return a list of operations and the new state of the contract storage.
The list can include any number of operations of any type.

As described in [Operations](https://docs.tezos.com/smart-contracts/logic/operations) on docs.tezos.com, operations do not run immediately when the operation object is created.
Instead, operations are added to a stack of operations to run after the code of the entrypoint is complete.
For example, if a contract checks its balance with the `Tezos.Next.get_balance` function, creates an operation to transfer tez to another account, and then checks its balance again in the same entrypoint execution, the balance is the same because the transfer operation has not run yet.
For more detailed examples, see [Operations](https://docs.tezos.com/smart-contracts/logic/operations) on docs.tezos.com.

There are no literal values of type operation.
Instead, such values are created using the following functions from the standard library: `Tezos.Next.Operation.transaction` (transfer), `Tezos.Next.Operation.create_contract` (origination), `Tezos.Next.Operation.set_delegate` (delegation), and `Tezos.Next.Operation.Emit` (emission of event).
For the operation to run, these operation values must be included in the list of operations returned at the end of the entrypoint code.

## Creating transactions

The `Tezos.Next.Operation.transaction` function creates a transaction operation, which can be a call to a smart contract (including the same contract) or a transfer of tez to a user account (implicit account).
Its parameters are:

- The parameter to pass
- The amount of tez to send
- The address to call

### Sending tez

To send tez to a user account, pass `unit` as the parameter and the address of the account as the address to call, as in this example, which sends 5 tez to the account that calls it:

<Syntax syntax="cameligo">

```cameligo group=send_tez
type storage = unit
type return_value = operation list * storage

[@entry] let give5tez (_ : unit) (storage : storage) : return_value =
  if Tezos.Next.get_balance () >= 5tez then
    let receiver_contract = match Tezos.Next.get_contract_opt (Tezos.Next.get_sender ()) with
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
  if (Tezos.Next.get_balance() >= 5tez) {
    const receiver_contract = match(Tezos.Next.get_contract_opt(Tezos.Next.get_sender())) {
      when(Some(contract)): contract;
      when(None): failwith("Couldn't find account");
    };
    operations = [Tezos.Next.Operation.transaction(unit, 5tez, receiver_contract)];
  }
  return [operations, storage];
}
```

</Syntax>

### Calling a contract

To call another contract, you need to get the parameter to pass to the contract, which includes the entrypoint, if the contract uses them.
The following example shows two contracts, A and B.
B stores contract A's address and calls its entrypoints.
To get the correct parameter for the transaction, contract B uses the `parameter_of` keyword to create a parameter that represents a call to contract A's entrypoints.

<Syntax syntax="cameligo">

```cameligo group=operation_transaction
module Test = Test.Next

type 'storage return = operation list * 'storage

module A = struct
    type storage = int

    [@entry]
    let add (delta : int) (storage : storage) : storage return =
      [], storage + delta

    [@entry]
    let sub (delta : int) (storage : storage) : storage return =
      [], storage - delta
  end

module B = struct
    type storage = address

    [@entry]
    let increment (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_contract stored_address in
      let parameter : A parameter_of = Add value in
      let operation = Tezos.Next.Operation.transaction parameter 0tez contract in
    [operation], stored_address

    [@entry]
    let decrement (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_contract stored_address in
      let parameter : A parameter_of = Sub value in
      let operation = Tezos.Next.Operation.transaction parameter 0tez contract in
    [operation], stored_address
  end

let test =
  // Originate contract A
  let contract_A = Test.Originate.contract (contract_of A) 0 0tez in
  let contract_A_address = Test.Typed_address.to_address contract_A.taddr in

  // Originate contract B with the address of contract A in its storage
  let contract_B = Test.Originate.contract (contract_of B) contract_A_address 0tez in

  // Call contract B
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "increment" contract_B.taddr) 10 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "decrement" contract_B.taddr) 2 0tez in

  let newNumber = Test.Typed_address.get_storage contract_A.taddr in
  Assert.assert (newNumber = 8)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=operation_transaction
import Test = Test.Next;

type @return<storage> = [list<operation>, storage];

namespace A {
  type storage = int;

  @entry
  const add = (delta: int, storage: storage): @return<storage> =>
    [[], storage + delta];

  @entry
  const sub = (delta: int, storage: storage): @return<storage> =>
    [[], storage - delta];
}

namespace B {
  type storage = address;

  @entry
  const increment = (value: int, stored_address: storage): @return<storage> => {
    const contract = Tezos.Next.get_contract(stored_address);
    const parameter = Add(value) as parameter_of A;
    const operation = Tezos.Next.Operation.transaction(parameter, 0tez, contract);
    return [[operation], stored_address];
  }

  @entry
  const decrement = (value: int, stored_address: storage): @return<storage> => {
    const contract = Tezos.Next.get_contract(stored_address);
    const parameter = Sub(value) as parameter_of A;
    const operation = Tezos.Next.Operation.transaction(parameter, 0tez, contract);
    return [[operation], stored_address];
  }
}

const test = () => {
  // Originate contract A
  const contract_A = Test.Originate.contract(contract_of(A), 0, 0tez);
  const contract_A_address = Test.Typed_address.to_address(contract_A.taddr);

  // Originate contract B with the address of contract A in its storage
  const contract_B = Test.Originate.contract(contract_of(B), contract_A_address, 0tez);

  // Call contract B
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("increment", contract_B.taddr), 10 as int, 0tez);
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("decrement", contract_B.taddr), 2 as int, 0tez);

  const newNumber = Test.Typed_address.get_storage(contract_A.taddr);
  Assert.assert(newNumber == 8);
}

const result = test();
```

</Syntax>

If you don't have the LIGO code of the contract to use the `parameter_of` keyword, you can often get the information to call the contract from the code of the deployed contract.
For example, contract A in the previous example compiles to this Michelson code:

```michelson
{ parameter (or (int %sub) (int %add)) ;
  storage int ;
  code { UNPAIR ; IF_LEFT { SWAP ; SUB } { ADD } ; NIL operation ; PAIR } }
```

Michelson contracts don't have separate code for each entrypoint; instead, they accept a parameter that indicates which code to run.
In most cases, the parameter is annotated with the names of the entrypoints from the source code, whether that source code is in LIGO or another high-level Tezos language.
In this case, the annotations `%sub` and `%add` indicate the parameters to pass to run the code from the `sub` and `add` entrypoints from the source code.

LIGO can use these annotations to parse the parameter and format the call to the contract.
For example, this contract uses the `Tezos.Next.get_entrypoint` function to create a contract address that includes the parameter that indicates the entrypoint.
Then it passes the parameter value for the entrypoint without the entrypoint name as the first parameter of the `Tezos.Next.Operation.transaction` function:

<Syntax syntax="cameligo">

```cameligo group=operation_transaction
module C = struct
    type storage = address

    [@entry]
    let increment (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_entrypoint "%add" stored_address in
      let operation = Tezos.Next.Operation.transaction value 0tez contract in
    [operation], stored_address

    [@entry]
    let decrement (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_entrypoint "%sub" stored_address in
      let operation = Tezos.Next.Operation.transaction value 0tez contract in
    [operation], stored_address
  end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=operation_transaction
namespace C {
  type storage = address;

  @entry
  const increment = (value: int, stored_address: storage): @return<storage> => {
    const contract = Tezos.Next.get_entrypoint("%add", stored_address);
    const operation = Tezos.Next.Operation.transaction(value, 0tez, contract);
    return [[operation], stored_address];
  }

  @entry
  const decrement = (value: int, stored_address: storage): @return<storage> => {
    const contract = Tezos.Next.get_entrypoint("%sub", stored_address);
    const operation = Tezos.Next.Operation.transaction(value, 0tez, contract);
    return [[operation], stored_address];
  }
}
```

</Syntax>

If you don't have the source code or annotated parameter of the target contract, you must create the parameter as described in [Interoperability](../../tezos/contracts/interop).

For example, the parameter of contract A minus the annotations looks like this:

```michelson
(or (int) (int))
```

This code means that the contract accepts the parameter `(Left int)` or `(Right int)`, which correspond to the `sub` and `add` entrypoints in contract A.
For example, to pass 5 to the code for the `sub` entrypoint, the client passes `(Left 5)` to the contract.

You can construct this parameter in LIGO code with the `michelson_pair` and `michelson_or` types.
In this case, the parameter is a Michelson option type that takes a  `M_left` or `M_right` value.
For example, to create the parameter `(Left 5)`, use this code:

<Syntax syntax="cameligo">

```cameligo skip
type contract_a_param = (int, "sub", int, "add") michelson_or
let pass_5_to_sub : contract_a_param = M_left 5
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
type contract_a_param = michelson_or<[int, "sub", int, "add"]>;
const pass_5_to_sub: contract_a_param = M_left(5);
```

</Syntax>

This example contract uses this method to construct the parameter for contract A from the Michelson value and then uses that parameter to call it:

<Syntax syntax="cameligo">

```cameligo group=operation_transaction
module D = struct
  type storage = address
  type contract_a_param = (int, "sub", int, "add") michelson_or

  [@entry]
  let increment (value : int) (stored_address : storage) : storage return =
    let pass_to_add : contract_a_param = M_right value in
    let contract = Tezos.Next.get_contract stored_address in
    let operation = Tezos.Next.Operation.transaction pass_to_add 0tez contract in
  [operation], stored_address

  [@entry]
  let decrement (value : int) (stored_address : storage) : storage return =
    let pass_to_sub : contract_a_param = M_left value in
    let contract = Tezos.Next.get_contract stored_address in
    let operation = Tezos.Next.Operation.transaction pass_to_sub 0tez contract in
  [operation], stored_address
end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=operation_transaction
namespace D {
  type storage = address;
  type contract_a_param = michelson_or<[int, "sub", int, "add"]>;

  @entry
  const increment = (value: int, stored_address: storage): @return<storage> => {
    const pass_to_add: contract_a_param = M_right(value);
    const contract = Tezos.Next.get_contract(stored_address);
    const operation = Tezos.Next.Operation.transaction(pass_to_add, 0tez, contract);
    return [[operation], stored_address];
  }

  @entry
  const decrement = (value: int, stored_address: storage): @return<storage> => {
    const pass_to_sub: contract_a_param = M_left(value);
    const contract = Tezos.Next.get_contract(stored_address);
    const operation = Tezos.Next.Operation.transaction(pass_to_sub, 0tez, contract);
    return [[operation], stored_address];
  }
}
```

</Syntax>

For information about constructing more complicated parameters, see [Interoperability](../../tezos/contracts/interop).

## Originating contracts

The `Tezos.Next.Operation.create_contract` function creates an operation to originate a contract.
Its parameters are:

- The code of the new contract as a function
- The delegate for the new contract, as an option
- The amount of tez for the contract's initial balance
- The initial storage value for the contract

The `Tezos.Next.Operation.create_contract` function returns the operation and the address of the new contract.
However, a contract cannot originate a contract and call it in the same entrypoint execution because the origination operation must run first, and as described previously, operations do not run until the entrypoint execution is complete.
Calling the `Tezos.Next.get_contract_opt` function on that address returns `None` until the new contract is actually originated.

This example originates a simple contract:

<Syntax syntex="cameligo">

```cameligo group=origination
type return = operation list * string

[@entry]
let main (_ : string) (storage : string) : return =
  let entrypoint (_ : nat) (storage : string) =
    (([] : operation list), storage) in
  let op, _addr : operation * address =
    Tezos.Next.Operation.create_contract
      entrypoint
      (None : key_hash option)
      300000000mutez
      "one"
  in [op], storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=origination
type @return = [list<operation>, string];

@entry
const main = (_: string, storage: string) : @return => {
  const entrypoint = (_param: nat, storage: string) =>
    [list([]), storage];
  const [op, _addr]: [operation, address] =
    Tezos.Next.Operation.create_contract(entrypoint,
                          (None() as option<key_hash>),
                          300000000mutez,
                          "one");
  return [[op], storage];
}
```

</Syntax>

## Delegation

The `Tezos.Next.Operation.set_delegate` function creates an operation that changes the delegate for the current contract.
Its parameter is an option with the public key hash of the new delegate or `None` to withdraw delegation.
The operation (not the function itself) fails if the new key hash is the same as the current delegate or is not registered as a delegate.

<Syntax syntax="cameligo">

```cameligo group=set_delegate
[@entry]
let changeDelegate (new_delegate : key_hash) (storage : unit) : operation list * unit =
  [Tezos.Next.Operation.set_delegate (Some new_delegate)], storage
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=set_delegate
@entry
const changeDelegate = (new_delegate: key_hash, storage: unit): [list<operation>, unit] =>
  [[Tezos.Next.Operation.set_delegate (Some(new_delegate))], storage];
```

</Syntax>


## Emitting events

The `Tezos.Next.Operation.Emit` function creates an event emission operation.
Its parameters are the tag for the event and the payload for the event.
For more information about events, see [Events](../../contract/events).

<Syntax syntax="cameligo">

```cameligo group=event_emit
[@entry]
let emitEvents (_ : unit) (storage : int) : operation list * int =
  let event1 : operation = Tezos.Next.Operation.emit "%emitEvents" "hi" in
  let event2 : operation = Tezos.Next.Operation.emit "%emitEvents" 6 in
  [event1; event2], storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=event_emit
@entry
const emitEvents = (_: unit, storage: int): [list<operation>, int] => {
  const event1: operation = Tezos.Next.Operation.emit("%emitEvents", "hi");
  const event2: operation = Tezos.Next.Operation.emit("%emitEvents", 6);
  return [[event1, event2], storage];
}
```

</Syntax>
