---
title: "Part 2: Testing the contract"
---

It's critical to test contracts before you deploy them because they cannot be changed after you deploy them.
LIGO includes automated testing tools that let you test contracts and verify that they work the way you intend before you deploy them.
In this section, you add tests for the `buy_taco` entrypoint.

## Creating tests

You can put tests in the same file as the contract or in a different file.
For convenience, in this tutorial, you put the tests in the same file.

<Syntax syntax="jsligo">

1. At the top of the contract file, outside of the namespace, add this code to import the new version of LIGO testing tools:

   ```jsligo skip
   import Test = Test.Next;
   import Tezos = Tezos.Next;
   ```

1. At the end of the contract file, outside of the namespace, add this convenience function to call the view and get the current price of a taco:

   ```jsligo skip
   // Convenience function to get current taco price
   const get_taco_price = (untyped_address: address, taco_kind_index: nat): tez => {
     const view_result_option: option<tez> = Tezos.View.call("get_taco_price", taco_kind_index, untyped_address);
     return $match(view_result_option, {
       "Some": (cost_mutez) => cost_mutez,
       "None": () => Test.failwith("Couldn't get the price of the taco."),
     });
   }
   ```

1. Add this convenience function to verify the current stock and maximum price of a taco:

   ```jsligo skip
   // Convenience function for testing equality in maps
   const eq_in_map = (r: TacoShop.taco_supply, m: TacoShop.taco_data, k: nat) =>
     $match(Map.find_opt(k, m), {
       "None": () => false,
       "Some": (v) => v.current_stock == r.current_stock && v.max_price == r.max_price
     });
   ```

   This function accepts information about a taco type and verifies that the values in the stored map match.

1. At the end of the contract file, outside of the namespace, add this stub of a function to hold the test logic:

   ```jsligo skip
   const test = (() => {

     // Test logic goes here

   }) ();
   ```

1. Inside the test function, add code to deploy the contract in the test scenario:

   ```jsligo skip
   // Set the initial storage and deploy the contract
   const admin_address: address = Test.Account.address(0 as nat);
   const initial_storage: TacoShop.storage = {
     admin_address: admin_address,
     taco_data: TacoShop.default_taco_data,
   }
   const contract = Test.Originate.contract(contract_of(TacoShop), initial_storage, 0 as tez);
   ```

   This code creates the `contract` object to represent the deployed (originated) contract.
   This object has a few fields, but the main one the test uses is the `taddr` field, which is the address of the deployed contract.
   Now you can call the deployed contract in the test scenario.

1. Get the current price of one kind of taco by calling the `get_taco_price` function:

   ```jsligo skip
   // Get the current price of a taco
   const untyped_address = Test.Typed_address.to_address(contract.taddr);
   const current_price = get_taco_price(untyped_address, 1 as nat);
   ```

1. Call the `buy_taco` entrypoint with this code:

   ```jsligo skip
   // Purchase a taco
   const success_result =
     Test.Contract.transfer(
       Test.Typed_address.get_entrypoint("buy_taco", contract.taddr),
       1 as nat,
       current_price
     );
   ```

   The `Test.Contract.transfer` function calls an entrypoint in a test scenario.
   It takes these parameters:

    1. The contract to call, here represented by the `buy_taco` entrypoint of the contract.
    1. The parameter to pass to the entrypoint, in this case `1 as nat` to represent the first type of taco.
    1. The amount of tez to send with the transaction, in this case the current price of that type of taco from the previous lines of code.

1. Verify that the transaction completed successfully and that the number of tacos of that type decreased by 1:

   ```jsligo skip
   // Verify that the stock was updated
   $match(success_result, {
     "Success": (_s) => (() => {
       const storage = Test.Typed_address.get_storage(contract.taddr);
       // Check that the stock has been updated correctly
       Assert.assert(
         eq_in_map(
           { current_stock: 49 as nat, max_price: 50000000 as mutez },
           storage.taco_data,
           1 as nat
         ));
       // Check that the amount of the other taco type has not changed
       Assert.assert(eq_in_map(
           { current_stock: 20 as nat, max_price: 75000000 as mutez },
           storage.taco_data,
           2 as nat
         )
       );
       Test.IO.log("Successfully bought a taco");
   })(),
     "Fail": (err) => failwith(err),
   });
   ```

1. Verify that the entrypoint fails when a client passes the wrong price:

   ```jsligo skip
   // Fail to purchase a taco without sending enough tez
   const fail_result =
     Test.Contract.transfer(
       Test.Typed_address.get_entrypoint("buy_taco", contract.taddr),
       1 as nat,
       1 as mutez
     );
   $match(fail_result, {
     "Success": (_s) => failwith("Test was able to buy a taco for the wrong price"),
     "Fail": (_err) => Test.IO.log("Contract successfully blocked purchase with incorrect price"),
   });
   ```

   It's important to test failure cases as well as success cases to make sure the contract works properly in all cases.

The completed contract file with convenience functions and test functions looks like this:

```jsligo group=testing_contract
import Test = Test.Next;
import Tezos = Tezos.Next;

namespace TacoShop {
  export type taco_supply = { current_stock: nat, max_price: tez };
  export type taco_data = map<nat, taco_supply>;
  export type admin_address = address;
  export type storage = {
    admin_address: admin_address,
    taco_data: taco_data,
  };

  export const default_taco_data: taco_data = Map.literal([
    [1 as nat, { current_stock: 50 as nat, max_price: 50 as tez }],
    [2 as nat, { current_stock: 20 as nat, max_price: 75 as tez }]
  ]);

  // Internal function to get the price of a taco
  const get_taco_price_internal = (taco_kind_index: nat, taco_data: taco_data): tez => {
    const taco_kind: taco_supply =
      $match (Map.find_opt(taco_kind_index, taco_data), {
        "Some": (kind) => kind,
        "None": () => failwith("Unknown kind of taco"),
      });
    return taco_kind.max_price / taco_kind.current_stock;
  }

  // @view
  const get_taco_price = (taco_kind_index: nat, storage: storage): tez =>
    get_taco_price_internal(taco_kind_index, storage.taco_data);

  // Buy a taco
  // @entry
  const buy_taco = (taco_kind_index: nat, storage: storage): [
      list<operation>,
      storage
    ] => {

    const { admin_address, taco_data } = storage;

    // Retrieve the kind of taco from the contracts storage or fail
    const taco_kind: taco_supply =
      $match (Map.find_opt(taco_kind_index, taco_data), {
        "Some": (kind) => kind,
        "None": () => failwith("Unknown kind of taco"),
      });

    // Get the current price of this type of taco
    const current_purchase_price = get_taco_price_internal(taco_kind_index, taco_data);

    // Verify that the caller sent the correct amount of tez
    if ((Tezos.get_amount()) != current_purchase_price) {
      return failwith("Sorry, the taco you are trying to purchase has a different price");
    }

    // Verify that there is at least one of this type of taco
    if (taco_kind.current_stock == (0 as nat)) {
      return failwith("Sorry, we are out of this type of taco");
    }

    // Update the storage with the new quantity of tacos
    const updated_taco_data: taco_data = Map.update(
      taco_kind_index,
      ["Some" as "Some", {...taco_kind, current_stock: abs(taco_kind.current_stock - 1) }],
      taco_data);

    const updated_storage: storage = {
      admin_address: admin_address,
      taco_data: updated_taco_data,
    };

    return [[], updated_storage];
  }

  // @entry
  const payout = (_u: unit, storage: storage): [
      list<operation>,
      storage
    ] => {

    // Entrypoint logic goes here

    return [[], storage];
  }

};

// Convenience function to get current taco price
const get_taco_price = (untyped_address: address, taco_kind_index: nat): tez => {
  const view_result_option: option<tez> = Tezos.View.call("get_taco_price", taco_kind_index, untyped_address);
  return $match(view_result_option, {
    "Some": (cost_mutez) => cost_mutez,
    "None": () => Test.failwith("Couldn't get the price of the taco."),
  });
}

// Convenience function for testing equality in maps
const eq_in_map = (r: TacoShop.taco_supply, m: TacoShop.taco_data, k: nat) =>
  $match(Map.find_opt(k, m), {
    "None": () => false,
    "Some": (v) => v.current_stock == r.current_stock && v.max_price == r.max_price
  });

const test = (() => {

  // Set the initial storage and deploy the contract
  const admin_address: address = Test.Account.address(0 as nat);
  const initial_storage: TacoShop.storage = {
    admin_address: admin_address,
    taco_data: TacoShop.default_taco_data,
  }
  const contract = Test.Originate.contract(contract_of(TacoShop), initial_storage, 0 as tez);

  // Get the current price of a taco
  const untyped_address = Test.Typed_address.to_address(contract.taddr);
  const current_price = get_taco_price(untyped_address, 1 as nat);

  // Purchase a taco
  const success_result =
    Test.Contract.transfer(
      Test.Typed_address.get_entrypoint("buy_taco", contract.taddr),
      1 as nat,
      current_price
    );

  // Verify that the stock was updated
  $match(success_result, {
    "Success": (_s) => (() => {
      const storage = Test.Typed_address.get_storage(contract.taddr);
      // Check that the stock has been updated correctly
      Assert.assert(
        eq_in_map(
          { current_stock: 49 as nat, max_price: 50000000 as mutez },
          storage.taco_data,
          1 as nat
        ));
      // Check that the amount of the other taco type has not changed
      Assert.assert(eq_in_map(
          { current_stock: 20 as nat, max_price: 75000000 as mutez },
          storage.taco_data,
          2 as nat
        )
      );
      Test.IO.log("Successfully bought a taco");
  })(),
    "Fail": (err) => failwith(err),
  });

  // Fail to purchase a taco without sending enough tez
  const fail_result =
    Test.Contract.transfer(
      Test.Typed_address.get_entrypoint("buy_taco", contract.taddr),
      1 as nat,
      1 as mutez
    );
  $match(fail_result, {
    "Success": (_s) => failwith("Test was able to buy a taco for the wrong price"),
    "Fail": (_err) => Test.IO.log("Contract successfully blocked purchase with incorrect price"),
  });
}) ();
```

</Syntax>

<Syntax syntax="cameligo">

1. At the top of the contract file, outside of the module, add this code to import the new version of LIGO testing tools:

   ```cameligo skip
   module Test = Test.Next
   module Tezos = Tezos.Next
   ```

1. At the end of the contract file, outside of the module, add this convenience function to call the view and get the current price of a taco:

   ```cameligo skip
   (* Convenience function to get current taco price *)
   let get_taco_price (untyped_address : address) (taco_kind_index : nat) : tez =
     let view_result_option : tez option = Tezos.View.call
       "get_taco_price"
       taco_kind_index
       untyped_address in
     match view_result_option with
     | Some cost_mutez -> cost_mutez
     | None -> Test.failwith "Couldn't get the price of a taco"
   ```

1. Add this convenience function to verify the current stock and maximum price of a taco:

   ```cameligo skip
   (* Convenience function for testing equality in maps *)
   let eq_in_map (r : TacoShop.taco_supply) (m : TacoShop.taco_data) (k : nat) =
     match Map.find_opt k m with
     | None -> false
     | Some v -> v.current_stock = r.current_stock && v.max_price = r.max_price
   ```

   This function accepts information about a taco type and verifies that the values in the stored map match.

1. At the end of the contract file, outside of the namespace, create a function to hold the test logic:

   ```cameligo skip
   let test =
   ```

1. Inside the test function, add code to deploy the contract in the test scenario:

   ```cameligo skip
   (* Set the initial storage and deploy the contract *)
   let admin_address : address = Test.Account.address 0n in
   let initial_storage : TacoShop.storage = {
     admin_address = admin_address;
     taco_data = TacoShop.default_taco_data
   } in
   let contract = Test.Originate.contract (contract_of TacoShop) initial_storage 0tez in
   ```

   This code creates the `contract` object to represent the deployed (originated) contract.
   This object has a few fields, but the main one the test uses is the `taddr` field, which is the address of the deployed contract.
   Now you can call the deployed contract in the test scenario.

1. Get the current price of one kind of taco by calling the `get_taco_price` function:

   ```cameligo skip
   (* Get the current price of a taco *)
   let untyped_address = Test.Typed_address.to_address contract.taddr in
   let current_price = get_taco_price untyped_address 1n in
   ```

1. Call the `buy_taco` entrypoint with this code:

   ```cameligo skip
   (* Purchase a taco *)
   let success_result =
     Test.Contract.transfer
       (Test.Typed_address.get_entrypoint "buy_taco" contract.taddr)
       1n
       current_price
     in
   ```

   The `Test.Contract.transfer` function calls an entrypoint in a test scenario.
   It takes these parameters:

      1. The contract to call, here represented by the `buy_taco` entrypoint of the contract.
      1. The parameter to pass to the entrypoint, in this case `1n` to represent the first type of taco.
      1. The amount of tez to send with the transaction, in this case the current price of that type of taco from the previous lines of code.

1. Verify that the transaction completed successfully and that the number of tacos of that type decreased by 1:

   ```cameligo skip
   (* Verify that the stock was updated *)
   let () = match success_result with
     | Success _s ->
       let storage = Test.Typed_address.get_storage contract.taddr in
       let () = Assert.assert (eq_in_map
         { current_stock = 49n; max_price = 50000000mutez }
         storage.taco_data
         1n
       ) in
       let () = Assert.assert (eq_in_map
         { current_stock = 20n; max_price = 75000000mutez }
         storage.taco_data
         2n
       ) in
       Test.IO.log "Successfully bought a taco"
     | Fail err -> failwith err
     in
   ```

1. Verify that the entrypoint fails when a client passes the wrong price:

   ```cameligo skip
   (* Fail to purchase a taco without sending enough tez *)
   let fail_result = Test.Contract.transfer
     (Test.Typed_address.get_entrypoint "buy_taco" contract.taddr)
     1n
     1mutez in
   match fail_result with
   | Success _s -> failwith "Test was able to buy a taco for the wrong price"
   | Fail _err -> Test.IO.log "Contract successfully blocked purchase with incorrect price"
   ```

   It's important to test failure cases as well as success cases to make sure the contract works properly in all cases.

The completed contract file with the convenience functions and test functions looks like this:

```cameligo group=testing_contract
module Test = Test.Next
module Tezos = Tezos.Next

module TacoShop = struct

  type taco_supply = { current_stock: nat; max_price: tez }
  type taco_data = (nat, taco_supply) map
  type admin_address = address
  type storage = {
    admin_address: admin_address;
    taco_data: taco_data;
  }

  let default_taco_data: taco_data = Map.literal [
    (1n, { current_stock = 50n; max_price = 50tez });
    (2n, { current_stock = 20n; max_price = 75tez });
  ]

  (* Internal function to get the price of a taco *)
  let get_taco_price_internal (taco_kind_index : nat) (taco_data : taco_data) : tez =
    let taco_kind : taco_supply =
      match Map.find_opt taco_kind_index taco_data with
      | Some kind -> kind
      | None -> failwith "Unknown kind of taco"
      in
      taco_kind.max_price / taco_kind.current_stock

  [@view]
  let get_taco_price (taco_kind_index : nat) (storage : storage) : tez =
    get_taco_price_internal taco_kind_index storage.taco_data

  (* Buy a taco *)
  [@entry]
  let buy_taco (taco_kind_index : nat) (storage : storage) : operation list * storage =

    let { admin_address; taco_data } = storage in

    (* Retrieve the kind of taco from the contracts storage or fail *)
    let taco_kind : taco_supply =
      match Map.find_opt taco_kind_index taco_data with
      | Some kind -> kind
      | None -> failwith "Unknown kind of taco" in

    (* Get the current price of this type of taco *)
    let current_purchase_price = get_taco_price_internal taco_kind_index taco_data in

    (* Verify that the caller sent the correct amount of tez *)
    let _ = if (Tezos.get_amount () <> current_purchase_price) then
      failwith "Sorry, the taco you are trying to purchase has a different price" in

    (* Verify that there is at least one of this type of taco *)
    let _ = if (taco_kind.current_stock = 0n) then
      failwith "Sorry, we are out of this type of taco" in


    (* Update the storage with the new quantity of tacos *)
    let updated_taco_data : taco_data = Map.update
      taco_kind_index
      (Some { taco_kind with current_stock = abs (taco_kind.current_stock - 1n) })
      taco_data in


    let updated_storage : storage = {
      admin_address = admin_address;
      taco_data = updated_taco_data;
    } in

    [], updated_storage

    [@entry]
    let payout (_u : unit) (storage : storage) : operation list * storage =

      (* Entrypoint logic goes here *)

      [], storage

  end

(* Convenience function to get current taco price *)
let get_taco_price (untyped_address : address) (taco_kind_index : nat) : tez =
  let view_result_option : tez option = Tezos.View.call
    "get_taco_price"
    taco_kind_index
    untyped_address in
  match view_result_option with
  | Some cost_mutez -> cost_mutez
  | None -> Test.failwith "Couldn't get the price of a taco"

(* Convenience function for testing equality in maps *)
let eq_in_map (r : TacoShop.taco_supply) (m : TacoShop.taco_data) (k : nat) =
  match Map.find_opt k m with
  | None -> false
  | Some v -> v.current_stock = r.current_stock && v.max_price = r.max_price

let test =

  (* Set the initial storage and deploy the contract *)
  let admin_address : address = Test.Account.address 0n in
  let initial_storage : TacoShop.storage = {
    admin_address = admin_address;
    taco_data = TacoShop.default_taco_data
  } in
  let contract = Test.Originate.contract (contract_of TacoShop) initial_storage 0tez in

  (* Get the current price of a taco *)
  let untyped_address = Test.Typed_address.to_address contract.taddr in
  let current_price = get_taco_price untyped_address 1n in

  (* Purchase a taco *)
  let success_result =
    Test.Contract.transfer
      (Test.Typed_address.get_entrypoint "buy_taco" contract.taddr)
      1n
      current_price
    in

  (* Verify that the stock was updated *)
  let () = match success_result with
    | Success _s ->
      let storage = Test.Typed_address.get_storage contract.taddr in
      let () = Assert.assert (eq_in_map
        { current_stock = 49n; max_price = 50000000mutez }
        storage.taco_data
        1n
      ) in
      let () = Assert.assert (eq_in_map
        { current_stock = 20n; max_price = 75000000mutez }
        storage.taco_data
        2n
      ) in
      Test.IO.log "Successfully bought a taco"
    | Fail err -> failwith err
    in

    (* Fail to purchase a taco without sending enough tez *)
    let fail_result = Test.Contract.transfer
      (Test.Typed_address.get_entrypoint "buy_taco" contract.taddr)
      1n
      1mutez in
    match fail_result with
    | Success _s -> failwith "Test was able to buy a taco for the wrong price"
    | Fail _err -> Test.IO.log "Contract successfully blocked purchase with incorrect price"
```

</Syntax>

## Running tests

LIGO tests do not run automatically when you run the `ligo compile contract` command; you must run them with the `ligo run test` command.

Run the tests in the contract file by running this command:

<Syntax syntax="jsligo">

```bash
ligo run test taco_shop.jsligo
```

</Syntax>

<Syntax syntax="cameligo">

```bash
ligo run test taco_shop.mligo
```

</Syntax>

The console response prints the messages from the calls to `Test.IO.log` and a message that the test function completed:

```
"Successfully bought a taco"
"Contract successfully blocked purchase with incorrect price"
Everything at the top-level was executed.
- test exited with value ().
```

If you want to expand the tests for your contract, you can add more test functions or more test code to the existing function.
For example, you can try buying the other kind of taco or buying more of the first kind of taco and verifying that the stock and price changes as expected.

## Testing with dry-run

Another way to test contracts is with the `ligo run dry-run` command.
This command runs the contract in a simulated environment with parameters that you provide on the command line.
You pass these arguments to the command:

- The contract file to run
- The amount of tez to pass with the transaction
- The parameter to pass to the contract, as a LIGO expression
- The value of the contract storage, as a LIGO expression

For example, you can test the `buy_taco` entrypoint with this command:

<Syntax syntax="jsligo">

```bash
ligo run dry-run taco_shop.jsligo -m TacoShop --amount 1 '["Buy_taco" as "Buy_taco", 1 as nat]' \
  '{admin_address: "tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx" as address, taco_data: TacoShop.default_taco_data}'
```

The entrypoint and parameter in this command are formatted as a variant type.
When the contract is compiled to Michelson, its parameter is a variant that has cases for each entrypoint, so you must pass the variant that corresponds to the entrypoint.
For the purposes of the `ligo run dry-run` command, the variant type is the name of the entrypoint with the first letter in upper case.
Note also that you can use variables from the contract (as in `TacoShop.default_taco_data`) in the command because the contract parameter and storage value are LIGO expressions.

</Syntax>

<Syntax syntax="cameligo">

```bash
ligo run dry-run taco_shop.mligo -m TacoShop --amount 1 "Buy_taco 1n" \
  '{admin_address = "tz1QCVQinE8iVj1H2fckqx6oiM85CNJSK9Sx" ; taco_data = TacoShop.default_taco_data}'
```

Note that the entrypoint name starts with a capital letter when you use it in a dry run.
Note also that you can use variables from the contract (as in `TacoShop.default_taco_data`) in the command because the contract parameter and storage value are LIGO expressions.

</Syntax>

The address in the dry run command isn't stored beyond this run of the command; you just need to provide any address for the amin address in storage.
However, you must use the `as address` declaration to specify that the string is a LIGO `address` type; without the type declaration, LIGO would assume that it was a string.

The output of the command is the return value of the entrypoint that you called.
In this case, it is an empty list of operations (`LIST_EMPTY()`) and the code for the new state of the storage.

In this way, you can test different storage states and entrypoints from the command line.
Sometimes testing with a dry run can be more convenient than writing tests; it's up to you how to test the contract.

Now you know that the customer interface of the contract works.
In the next section, you implement the `payout` entrypoint to retrieve the profits.
Continue to [Part 3: Getting the payouts](./getting-payouts).
