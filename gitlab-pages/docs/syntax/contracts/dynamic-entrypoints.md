---
id: dynamic-entrypoints
title: Dynamic entrypoints
---

import Syntax from '@theme/Syntax';

Dynamic entrypoints are lambda functions stored in a contract within a big_map.
A contract can update its dynamic entrypoints without deploying a new contract.
In this way, contracts can use dynamic entrypoints to change their internal logic.

:::note

Only the contract itself can call its dynamic entrypoints, not clients or other contracts.
If you want a client or other contract to be able to call a dynamic entrypoint, you can create an ordinary non-dynamic entrypoint as a wrapper for the dynamic entrypoint.

:::

A contract with dynamic entrypoints must have at least one non-dynamic entrypoint with the `@entry` declaration, like any other contract.
They must also obey the following convention on storage type definition and have at least one function with the `@dyn_entry` declaration.

## Storage

To contain dynamic entrypoints, the contract storage must be a record with two fields:

<Syntax syntax="cameligo">

- `storage`: The contract's storage, just like in an ordinary contract
- `dynamic_entrypoints`: The code of the dynamic entrypoints, which must be of the type `(nat,bytes) big_map`

</Syntax>

<Syntax syntax="jsligo">

- `storage`: The contract's storage, just like in an ordinary contract
- `dynamic_entrypoints`: The code of the dynamic entrypoints, which must be of the type `big_map<nat, bytes>`

</Syntax>

For convenience, the type of the `dynamic_entrypoints` storage field is defined as `Dynamic_entrypoints.t` in the standard library.

The LIGO compiler can generate the initial value of the storage for you; see [Compiling dynamic entrypoints](#compiling-dynamic-entrypoints).

## Defining dynamic entrypoints

To set the dynamic entrypoints that the contract has at origination time, define functions at the top level of the contract just like ordinary entrypoints but with these differences:

<Syntax syntax="cameligo">

- They have the `@dyn_entry` attribute instead of the `@entry` attribute
- Their storage and return types are different, as explained next

</Syntax>

<Syntax syntax="jsligo">

- They have the `@dyn_entry` decorator instead of the `@entry` decorator
- They must be exported with the `export` keyword
- Their storage and return types are different, as explained next

</Syntax>

Ordinary entrypoints in the contract receive a parameter and the value of the contract storage as usual, which, as described in [Storage](#storage), is always a record with a `storage` field and a `dynamic_entrypoints` field.
Also as usual, the ordinary entrypoints return a tuple with a list of operations and the new value of the storage, including both of these fields.

By contrast, dynamic entrypoints receive a parameter and the `storage` field of the contract storage, not the `dynamic_entrypoints` field.
Similarly, they return a tuple with a list of operations and the new value of that `storage` field.

:::note


Smart contracts cannot add dynamic entrypoints after the contract is originated.
Therefore, you must define all dynamic entrypoints in the contract source code.

:::

For example, this contract stores an integer but none of its ordinary entrypoints change it directly.
Instead, it contains dynamic entrypoints that manipulate it:

- The `double` dynamic entrypoint doubles the value in storage
- The `square` dynamic entrypoint squares the value in storage
- The `currentAction` dynamic entrypoint initially does nothing

The contract has two ordinary entrypoints that manipulate the dynamic entrypoints:

- The `runAction` entrypoint runs the `currentAction` dynamic entrypoint and updates the contract storage based on its result
- The `changeAction` entrypoint changes the `currentAction` dynamic entrypoint to be a copy of either the `double` or the `square` entrypoint

<Syntax syntax="cameligo">

```cameligo group=simple_dynamic
module DynamicContract = struct
  type internal_storage = int
  type storage_type = {
    storage : internal_storage;
    dynamic_entrypoints : Dynamic_entrypoints.t; // big_map<nat, bytes>
  }
  type return_type = operation list * storage_type
  type dyn_return_type = operation list * internal_storage

  // Dynamic entrypoint: double the integer in storage
  [@dyn_entry]
  let double (() : unit) (s : internal_storage) : dyn_return_type = [], s + s

  // Dynamic entrypoint: square the integer in storage
  [@dyn_entry]
  let square (() : unit) (s : internal_storage) : dyn_return_type = [], s * s

  // Initially, this dynamic entrypoint does nothing
  // But the changeAction entrypoint sets it to a different dynamic entrypoint
  [@dyn_entry]
  let currentAction (() : unit) (s : internal_storage) : dyn_return_type = [], s

  // Run the currentAction entrypoint
  [@entry]
  let runAction (() : unit) (full_storage : storage_type) : return_type =
    let {storage; dynamic_entrypoints} = full_storage in

    match (Dynamic_entrypoints.get currentAction dynamic_entrypoints) with
      Some f ->
        let (operations, newStorage) = f unit storage in
        operations, {
          storage = newStorage;
          dynamic_entrypoints = dynamic_entrypoints
        }
      | None -> failwith "Error"

  // Change the currentAction entrypoint to double or square
  [@entry]
  let changeAction (new_action_str : string) (full_storage : storage_type) : return_type =
    let {storage; dynamic_entrypoints} = full_storage in

    let new_dynamic_entrypoints : Dynamic_entrypoints.t =
      if (new_action_str = "double")
      then let new_action = match (Dynamic_entrypoints.get double dynamic_entrypoints) with
        | Some f -> f
        | None -> failwith "Error" in
        Dynamic_entrypoints.set currentAction (Some new_action) dynamic_entrypoints
      else let new_action = match (Dynamic_entrypoints.get square dynamic_entrypoints) with
        | Some f -> f
        | None -> failwith "Error" in
        Dynamic_entrypoints.set currentAction (Some new_action) dynamic_entrypoints in

    [], {
      storage = storage;
      dynamic_entrypoints = new_dynamic_entrypoints
    }

end
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=simple_dynamic
namespace DynamicContract {
  type internal_storage = int;
  type storage_type = {
    storage: internal_storage;
    dynamic_entrypoints: Dynamic_entrypoints.t; // map<nat, bytes>;
  };
  type return_type = [list<operation>, storage_type];
  type dyn_return_type = [list<operation>, internal_storage];

  // Dynamic entrypoint: double the integer in storage
  @dyn_entry
  export const double = (_u: unit, s: internal_storage): dyn_return_type => [[], s + s];

  // Dynamic entrypoint: square the integer in storage
  @dyn_entry
  export const square = (_u: unit, s: internal_storage): dyn_return_type => [[], s * s];

  // Initially, this dynamic entrypoint does nothing
  // But the changeAction entrypoint sets it to a different dynamic entrypoint
  @dyn_entry
  export const currentAction = (_u: unit, s: internal_storage): dyn_return_type => [[], s];

  // Run the currentAction entrypoint
  // @entry
  const runAction = (_u: unit, full_storage: storage_type): return_type => {
    const {storage, dynamic_entrypoints} = full_storage;

    return $match (Dynamic_entrypoints.get(currentAction, dynamic_entrypoints), {
      "Some": f => (() => {
        const [operations, newStorage] = f(unit, storage);
        return [operations, ({
          storage: newStorage,
          dynamic_entrypoints: dynamic_entrypoints
        })];
      })(),
      "None": () => failwith(-1)
    })
  }

  // Change the currentAction entrypoint to double or square
  // @entry
  const changeAction = (new_action_str: string, full_storage: storage_type): return_type => {
    const {storage, dynamic_entrypoints} = full_storage;

    let new_dynamic_entrypoints = dynamic_entrypoints;

    if (new_action_str == "double") {
      const new_action =
        $match(Dynamic_entrypoints.get(double, dynamic_entrypoints), {
          "Some": f => f,
          "None": () => failwith(-1)
        });
      new_dynamic_entrypoints =
        Dynamic_entrypoints.set(currentAction,
                                ["Some" as "Some", new_action],
                                dynamic_entrypoints);
    }

    if (new_action_str == "square") {
      const new_action =
        $match(Dynamic_entrypoints.get(square, dynamic_entrypoints), {
          "Some": f => f,
          "None": () => failwith(-1)
      });
      new_dynamic_entrypoints =
        Dynamic_entrypoints.set(currentAction,
                                ["Some" as "Some", new_action],
                                dynamic_entrypoints);
    }

    return [[], {
      storage: storage,
      dynamic_entrypoints: new_dynamic_entrypoints,
    }];
  }
}
```

</Syntax>

## Calling dynamic entrypoints

To call a dynamic entrypoint, use the function `Dynamic_entrypoints.get` to retrieve a dynamic entrypoint by name.
The function returns an option that contains a function that you can call like any other function.

This section from an earlier example attempts to retrieve the dynamic entrypoint named `currentAction` and runs it if it is found:

<Syntax syntax="cameligo">

```cameligo skip
match (Dynamic_entrypoints.get currentAction dynamic_entrypoints) with
  Some f ->
    let (operations, newStorage) = f unit storage in
    operations, {
      storage = newStorage;
      dynamic_entrypoints = dynamic_entrypoints
    }
  | None -> failwith "Error"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
return $match (Dynamic_entrypoints.get(currentAction, dynamic_entrypoints), {
  "Some": f => (() => {
    const [operations, newStorage] = f(unit, storage);
    return [operations, ({
      storage: newStorage,
      dynamic_entrypoints: dynamic_entrypoints
    })];
  })(),
  "None": () => failwith("Error")
})
```

</Syntax>

<Syntax syntax="cameligo">

Contracts cannot call dynamic entrypoints directly because they are stored as typed keys in the `dynamic_entrypoints` big map, not as functions.
LIGO uses an abstract type `('a,'b) dynamic_entrypoint` to denote such keys.

</Syntax>

<Syntax syntax="jsligo">

Contracts cannot call dynamic entrypoints directly because they are stored as typed keys in the `dynamic_entrypoints` big map, not as functions.
LIGO uses an abstract type `dynamic_entrypoint<a, b>` to denote such keys.

</Syntax>

## Updating dynamic entrypoints

After the contract is deployed, it can update its dynamic entrypoints by rewriting the `dynamic_entrypoints` field in its storage.

You can use the `Dynamic_entrypoints.set` function to get the updated value of this field by passing the current value, the name of a dynamic entrypoint to update, and the code of that dynamic entrypoint.
This function does not directly update the contract storage; you must use its return value as the new value of the `dynamic_entrypoints` field in the entrypoint return value.

This section from an earlier example changes the entrypoint named `currentAction` to the code of the entrypoint named `square`:

<Syntax syntax="cameligo">

```cameligo skip
if (new_action_str = "square")
then let new_action = match (Dynamic_entrypoints.get square dynamic_entrypoints) with
  | Some f -> f
  | None -> failwith "Error" in
  Dynamic_entrypoints.set currentAction (Some new_action) dynamic_entrypoints in

[], {
  storage = storage;
  dynamic_entrypoints = new_dynamic_entrypoints
}
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
if (new_action_str == "square") {
  const new_action =
    $match (Dynamic_entrypoints.get(square, dynamic_entrypoints), {
      "Some": f => f,
      "None": () => failwith("Error")
    });
  new_dynamic_entrypoints =
    Dynamic_entrypoints.set(currentAction,
                            ["Some" as "Some", new_action],
                            dynamic_entrypoints);
}

return [[], {
  storage: storage,
  dynamic_entrypoints: new_dynamic_entrypoints,
}];
```

</Syntax>

You can also set a dynamic entrypoint to new code by passing bytecode to the `Dynamic_entrypoints.set_bytes` function.
This function does not verify that the bytecode is valid, only that it is a valid LIGO bytes data type.
If the encoding is wrong, any call to `Dynamic_entrypoints.get` for the dynamic entrypoint fails.

For example, the dynamic entrypoint `double` from an earlier example compiles to the bytecode `0x0502000000250320093100000019035b0765055f036d035b020000000a03210312053d036d034200000000`.
To set a dynamic entrypoint to this bytecode, pass the name of the entrypoint and the bytecode to the `Dynamic_entrypoints.set_bytes` function, as in this example:

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let set_double_bytes (() : unit) (full_storage : storage_type) : return_type =
  let {storage; dynamic_entrypoints} = full_storage in
  let double_bytes : bytes = 0x0502000000250320093100000019035b0765055f036d035b020000000a03210312053d036d034200000000 in
  let new_dynamic_entrypoints = Dynamic_entrypoints.set_bytes currentAction (Some double_bytes) dynamic_entrypoints in
  [], {
    storage = storage;
    dynamic_entrypoints = new_dynamic_entrypoints
  }
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
// @entry
const set_double_bytes = (_: unit, full_storage: storage_type): return_type => {
  const {storage, dynamic_entrypoints} = full_storage;
  const double_bytes = 0x0502000000250320093100000019035b0765055f036d035b020000000a03210312053d036d034200000000 as bytes;
  const new_dynamic_entrypoints =
    Dynamic_entrypoints.set_bytes(currentAction,
                                  ["Some" as "Some", double_bytes],
                                  dynamic_entrypoints);

  return [[], {
    storage: storage,
    dynamic_entrypoints: new_dynamic_entrypoints,
  }];
}
```

</Syntax>

## Opted out dynamic entrypoints

Because a contract cannot add dynamic entrypoints, you must define all dynamic entrypoints in the contract source code.
If you want to include a dynamic entrypoint in the contract but provide the code for it later, you can make the function a no-op (a function that does nothing) and change it later or you can use the special expression `OPT_OUT_ENTRY`.
This expression makes the LIGO compiler include the entrypoint without any initial code.
If you call the entrypoint in a test before setting code for it, it behaves as a no-op.
If you call the entrypoint in a deployed contract before setting code for it, the operation fails.

<Syntax syntax="cameligo">

```cameligo skip
[@dyn_entry]
let opt_out (_i : int) (_s : internal_storage) : dyn_return_type =
  let _ = 1 in [%external ("OPT_OUT_ENTRY")]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
// @dyn_entry
const opt_out = (_i: int, _s : internal_storage) : dyn_return_type =>
  (External `OPT_OUT_ENTRY`)
```

</Syntax>

When you run a test with an opted out dynamic entrypoint like this, the compiler prints the message `unsupported primitive OPT_OUT_ENTRY`.
You can safely ignore this message.

## Compiling dynamic entrypoints

When you compile the storage for a contract with dynamic entrypoints, you provide only the value of the `storage` field, not the full value of the storage.
The compiler automatically compiles the dynamic entrypoints and provides the full value of the storage including the `dynamic_entrypoints` field.

For example, to compile the example contract in [Defining dynamic entrypoints](#defining-dynamic-entrypoints), pass only the value of the integer in storage, as in this example:

<Syntax syntax="cameligo">

```bash
ligo compile storage -m DynamicContract gitlab-pages/docs/syntax/contracts/src/dynamic-entrypoints/simple_dynamic.mligo 42
```

</Syntax>

<Syntax syntax="jsligo">

```bash
ligo compile storage -m DynamicContract gitlab-pages/docs/syntax/contracts/src/dynamic-entrypoints/simple_dynamic.jsligo 42
```

</Syntax>

The response is the full value of the contract storage.
You can use this value as the initial storage when you originate the contract.

```michelson
(Pair 42
      { Elt 0
            0x0502000000250320093100000019035b0765055f036d035b020000000a03210312053d036d034200000000 ;
        Elt 1
            0x0502000000250320093100000019035b0765055f036d035b020000000a0321033a053d036d034200000000 })
```

## Testing dynamic entrypoints

To simplify testing contracts with dynamic entrypoints, you can use
the function `Test.Dynamic_entrypoints.storage` to generate the
initial storage.  Like the `ligo compile storage` command, this
function takes only the value of the `storage` field in the contract
storage, not the full storage value.

It returns the full storage value with both the `storage` and `dynamic_entrypoints` field.
Then you can use this return value to originate the contract in the test.

After origination, testing a contract with dynamic entrypoints is the same as testing any other contract.
You cannot call the dynamic entrypoints directly in a test because this is the same behavior that the originated contract has.

For example, this is a test for the contract in [Defining dynamic entrypoints](#defining-dynamic-entrypoints):

<Syntax syntax="cameligo">

```cameligo group=simple_dynamic
let test_dyn =
  // Generate storage with dynamic entrypoints
  let initial_storage = Test.Dynamic_entrypoints.storage (contract_of DynamicContract) 3 in
  let contract = Test.Originate.contract (contract_of DynamicContract) initial_storage 0mutez in
  let storage_before = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage_before.storage 3) in

  // At the start, the runAction dynamic entrypoint does nothing
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  // Verify that storage did not change
  let () = Assert.assert (Test.Compare.eq storage storage_before) in

  // Set current action to double
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "changeAction" contract.taddr) "double" 0tez in

  // Double storage to 6
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage.storage 6) in

  // Double storage to 12
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage.storage 12) in

  // Switch to square
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "changeAction" contract.taddr) "square" 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  let () = Assert.assert (Test.Compare.eq storage.storage 12) in

  // Square storage to 144
  let _ : nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "runAction" contract.taddr) unit 0tez in
  let storage = Test.Typed_address.get_storage contract.taddr in
  Assert.assert(Test.Compare.eq storage.storage 144)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=simple_dynamic
const test_dyn = (() => {
  // Generate storage with dynamic entrypoints
  const initial_storage =
    Test.Dynamic_entrypoints.storage(contract_of(DynamicContract), 3);
  const contract =
  Test.Originate.contract(contract_of(DynamicContract),
                          initial_storage,
                          0 as mutez);
  const storage_before = Test.Typed_address.get_storage(contract.taddr);
  Assert.assert(Test.Compare.eq(storage_before.storage, 3));

  // At the start, the runAction dynamic entrypoint does nothing
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("runAction",
  contract.taddr), unit, 0 as tez);
  let storage = Test.Typed_address.get_storage(contract.taddr);
  // Verify that storage did not change
  Assert.assert(Test.Compare.eq(storage, storage_before));

  // Set current action to double
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("changeAction",
  contract.taddr), "double", 0 as tez);

  // Double storage to 6
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("runAction",
  contract.taddr), unit, 0 as tez);
  storage = Test.Typed_address.get_storage(contract.taddr);
  Assert.assert(Test.Compare.eq(storage.storage, 6));

  // Double storage to 12
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("runAction",
  contract.taddr), unit, 0 as tez);
  storage = Test.Typed_address.get_storage(contract.taddr);
  Assert.assert(Test.Compare.eq(storage.storage, 12));

  // Switch to square
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("changeAction",
  contract.taddr), "square", 0 as tez);
  storage = Test.Typed_address.get_storage(contract.taddr);
  Assert.assert(Test.Compare.eq(storage.storage, 12));

  // Square storage to 144
  Test.Contract.transfer_exn(Test.Typed_address.get_entrypoint("runAction",
  contract.taddr), unit, 0 as tez);
  storage = Test.Typed_address.get_storage(contract.taddr);
  Assert.assert(Test.Compare.eq(storage.storage, 144));
})()
```

</Syntax>
