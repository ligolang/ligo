---
id: views
title: Views
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

Views are a way for contracts to expose information to other contracts and to off-chain consumers.

Contracts can store the source code of their views either on-chain or off-chain:

- The code of on-chain views is stored in the smart contract code itself, like entrypoints.
- The code of off-chain views is stored externally, usually in decentralized data storage such as IPFS.
The contract metadata has information about its off-chain views that consumers such as indexers and other dApps use to know what off-chain views are available and to run them.

On-chain and off-chain views have the same capabilities and limitations.

For more information about views, see [Views](https://docs.tezos.com/smart-contracts/views) on docs.tezos.com.

## Defining on-chain views

<Syntax syntax="cameligo">

To define an on-chain view, use the `@view` attribute.

```cameligo group=onchainviews
type storage = string
type ret = operation list * storage

[@entry]
let main (word : string) (storage : storage) : ret
  = [] , storage ^ " " ^ word

(* This view returns the storage *)
[@view] let view1 (() : unit) (storage : storage) : storage
  = storage

(* This view returns true if the storage has a given length *)
[@view] let view2 (expected_length : nat) (storage : storage) : bool
  = (String.length storage = expected_length)

(* This view does not use the parameters or storage and returns a constant int *)
[@view] let view3 (() : unit) (_ : storage) : int
  = 42
```

</Syntax>

<Syntax syntax="jsligo">

To define an on-chain view, use the `@view` decorator.

```jsligo group=onchainviews
type storage = string
type ret = [list<operation>, storage];

@entry
const main = (word : string, storage : storage) : ret
  => [[] , storage + " " + word]

// This view returns the storage
@view
const view1 = (_arg : unit, storage : storage) : storage
  => storage;

// This view returns true if the storage has a given length
@view
const view2 = (expected_length : nat , storage : storage) : bool
  => (String.length (storage) == expected_length);

// This view does not use the parameters or storage and returns a constant int
@view
const view3 = (_arg : unit , _s : storage) : int
  => 42;
```

</Syntax>

## Defining off-chain views

To compile an off-chain view, create a function, compile it as an expression, and put the expression in the contract's metadata.

To compile an expression as a off-chain view, use the LIGO `compile expression` command and pass the `--function-body` flag.
To use an expression from a file, pass it in the `--init-file` argument.

For example, this file has a contract named `C` with a function named `v`:

<Syntax syntax="cameligo">

```cameligo group=view_file
module C = struct
  type storage = string

  [@entry] let append (a : string) (s : storage) : operation list * storage = [] , s ^ a

  [@entry] let clear (_ : unit) (_ : storage) : operation list * storage = [] , ""

  let v (expected_length: nat) (s: storage) : bool = (String.length s = expected_length)
end
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=view_file
namespace C {
  type storage = string

  @entry
  const append = (a: string, s: storage) : [list<operation> , storage] => [[], s + a];

  @entry
  const clear = (_p: unit, _s: storage) : [list<operation>, storage] => [[], ""];

  export const v = (expected_length: nat, s: storage) : bool => (String.length (s) == expected_length);
}
```

</Syntax>

To compile the function `v` as an off-chain view, pass `C.v` to the `compile expression` command, as in this example:

<Syntax syntax="cameligo">

```bash
ligo compile expression cameligo "C.v" --init-file off_chain.mligo --function-body
```

</Syntax>
<Syntax syntax="jsligo">

```bash
ligo compile expression jsligo "C.v" --init-file off_chain.jsligo --function-body
```

</Syntax>

The response is the function compiled to Michelson.
It is up to you to store this code and link to it from the contract metadata.
<!-- TODO how to set up off-chain views -->

```michelson
{ UNPAIR ; SWAP ; SIZE ; COMPARE ; EQ }
```

Note that the function is not annotated as an entrypoint or on-chain view; it is just a function declared in the context of the contract.

## Calling views

Contracts can call on-chain and off-chain views with the `Tezos.call_view` function and use the result immediately.

<SyntaxTitle syntax="cameligo">
val call_view : string -> 'arg -> address -> 'ret option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
const call_view : string => 'arg => address => option &lt;'ret&gt;
</SyntaxTitle>

The function accepts these parameters:

- The name of the view
- The parameter to pass to the view
- The address of the contract

<Syntax syntax="cameligo">

For example, this contract has a view that multiplies the integer in storage with the integer that the caller passes and returns the result:

```cameligo group=callonchainview
module ContractWithView = struct
  type storage = int
  type return_type = operation list * storage

  [@entry] let main (param : int) (_storage : storage) : return_type =
    [], param

  [@view] let multiply (param : int) (storage : storage) : int =
    param * storage

end
```

This contract stores the address of the first contract and calls its view:

```cameligo group=callonchainview
module CallView = struct
  type storage = address * int
  type return_type = operation list * storage

  [@entry] let callView (param : int) (storage : storage) : return_type =
    let (targetAddress, _s) = storage in
    let resultOpt : int option = Tezos.call_view "multiply" param targetAddress in
    match resultOpt with
      Some newValue -> [], (targetAddress, newValue)
    | None -> failwith("Something went wrong")
end
```

This test deploys both contracts, calls the contract that calls the view, and verifies the result:

```cameligo group=callonchainview
let test =

  // Originate ContractWithView
  let contract1 = Test.Next.Originate.contract (contract_of ContractWithView) 5 0tez in
  let addr1 = Test.Next.Typed_address.to_address contract1.taddr in

  // Originate CallView with address of ContractWithView in storage
  let initial_storage = (addr1, 0) in
  let contract2 = Test.Next.Originate.contract (contract_of CallView) initial_storage 0tez in

  // Call callView
  let _ : nat = Test.Next.Contract.transfer_exn (Test.Next.Typed_address.get_entrypoint "default" contract2.taddr) 12 0tez in
  let (_address, integer) = Test.Next.Typed_address.get_storage contract2.taddr in
  Assert.assert(integer = 60)
```

</Syntax>

<Syntax syntax="jsligo">

For example, this contract has a view that multiplies the integer in storage with the integer that the caller passes and returns the result:

```jsligo group=callonchainview
namespace ContractWithView {
  type storage = int;
  type return_type = [list<operation>, storage];

  @entry
  const main = (param: int, _storage: storage): return_type =>
    [[], param];

  @view
  const multiply = (param: int, storage: storage): int =>
    param * storage;
}
```

This contract stores the address of the first contract and calls its view:

```jsligo group=callonchainview
namespace CallView {
  type storage = [address, int];
  type return_type = [list<operation>, storage];

  @entry
  const callView = (param: int, storage: storage): return_type => {
    const [targetAddress, _s] = storage;
    const resultOpt: option<int> = Tezos.call_view(
      "multiply",
      param,
      targetAddress
    );
    return match(resultOpt) {
      when (None):
        failwith("Something went wrong");
      when (Some(newValue)):
        [[], [targetAddress, newValue]];
    }
  }
}
```

This test deploys both contracts, calls the contract that calls the view, and verifies the result:

```jsligo group=callonchainview
const test = (() => {

  // Originate ContractWithView
  const contract1 = Test.Next.Originate.contract(contract_of(ContractWithView), 5, 0tez);
  const addr1 = Test.Next.Typed_address.to_address(contract1.taddr);

  // Originate CallView with address of ContractWithView in storage
  const initial_storage = [addr1, 0 as int];
  const contract2 = Test.Next.Originate.contract(contract_of(CallView), initial_storage, 0tez);

  // Call callView
  Test.Next.Contract.transfer_exn(Test.Next.Typed_address.get_entrypoint("default", contract2.taddr), 12, 0tez);
  const [_address, integer] = Test.Next.Typed_address.get_storage(contract2.taddr);
  Assert.assert(integer == 60);
}) ()
```

</Syntax>

<!-- updated use of entry -->
