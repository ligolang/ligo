---
title: "Part 3: Getting the payouts"
pagination_next: null
---

Now that the customer-facing entrypoint of the contract is ready, you can set up the administrator-related entrypoint.
In this case, Pedro needs a way to reset the stock of tacos and send the tez from the contract to his account.
You could do this in two entrypoints, but for simplicity this tutorial shows how to do both of these things in one entrypoint named `payout`.

## Adding administrator information

Also for the sake of simplicity, the contract provides no way to change Pedro's account address after the contract is deployed.
In production applications, the address of the administrator should be in the contract storage and an entrypoint should allow the current administrator to change the administrator address.
As it is, this contract cannot change the administrator address after it is deployed, so use caution.

<Syntax syntax="jsligo">

1. In the `payout` entrypoint, add this code to verify that the administrator is calling the entrypoint:

   ```jsligo skip
   // Ensure that only the admin can call this entrypoint
   if (Tezos.get_sender() != storage.admin_address) {
     failwith("Only the admin can call this entrypoint");
   }
   ```

   The function `Tezos.get_sender` returns the address of the account that called the smart contract.

1. Add this code to generate the operation that sends tez to the administrator account:

   ```jsligo skip
   // Create contract object that represents the target account
   const receiver_contract = $match(Tezos.get_contract_opt(storage.admin_address), {
     "Some": (contract) => contract,
     "None": () => failwith("Couldn't find account"),
   });

   // Create operation to send tez
   const payout_operation = Tezos.Operation.transaction(unit, Tezos.get_balance(), receiver_contract);
   ```

   Sending tez to a user account means treating the user account as though it is a smart contract account.
   This way, sending tez to a user account works in the same way as sending tez to a smart contract.

   The `Tezos.Operation.transaction` function creates a Tezos transaction.
   There are many kinds of internal transactions in Tezos, but most smart contracts deal with these transactions:

      - Transferring tez to another account
      - Calling an entrypoint on a smart contract

   Calling an entrypoint on a smart contract (either the current contract or another contract) is beyond the scope of this tutorial.
   For information, see [Calling a contract](../../syntax/contracts/operation#calling-a-contract).

   The `Tezos.Operation.transaction` function takes these parameters:

   1. The parameter to pass, in this case `unit`, which means no value
   1. The amount of tez to include with the transaction, in this case all of the tez the contract has, denoted by the `Tezos.get_balance` function
   1. The address of the target contract

1. Add this code to calculate the new value of the storage, using the existing admin address and the default taco data:

   ```jsligo skip
   // Restore stock of tacos
   const new_storage: storage = {
     admin_address: storage.admin_address,
     taco_data: default_taco_data,
   };
   ```

1. Replace the `payout` entrypoint's `return` statement with this code:

   ```jsligo skip
   return [[payout_operation], new_storage];
   ```

   Creating the transaction is not enough to run it; you must return it in the list of operations at the end of the entrypoint.

The complete entrypoint looks like this:

```jsligo skip
// @entry
const payout = (_u: unit, storage: storage): [
    list<operation>,
    storage
  ] => {

  // Ensure that only the admin can call this entrypoint
  if (Tezos.get_sender() != storage.admin_address) {
    failwith("Only the admin can call this entrypoint");
  }

  // Create contract object that represents the target account
  const receiver_contract = $match(Tezos.get_contract_opt(storage.admin_address), {
    "Some": (contract) => contract,
    "None": () => failwith("Couldn't find account"),
  });

  // Create operation to send tez
  const payout_operation = Tezos.Operation.transaction(unit, Tezos.get_balance(), receiver_contract);

  // Restore stock of tacos
  const new_storage: storage = {
    admin_address: storage.admin_address,
    taco_data: default_taco_data,
  };

  return [[payout_operation], new_storage];
}
```

</Syntax>

<Syntax syntax="cameligo">

1. In the `payout` entrypoint, add this code to verify that the administrator is calling the entrypoint:

   ```cameligo skip
   (* Ensure that only the admin can call this entrypoint *)
   let _ = if (Tezos.get_sender () <> storage.admin_address) then
     failwith "Only the admin can call this entrypoint" in
   ```

   The function `Tezos.get_sender` returns the address of the account that called the smart contract.

1. Add this code to generate the operation that sends tez to the administrator account:

   ```cameligo skip
   (* Create contract object that represents the target account *)
   let receiver_contract = match Tezos.get_contract_opt storage.admin_address with
   | Some contract -> contract
   | None -> failwith "Couldn't find account" in

   (* Create operation to send tez *)
   let payout_operation = Tezos.Operation.transaction unit (Tezos.get_balance ()) receiver_contract in
   ```

   Sending tez to a user account means treating the user account as though it is a smart contract account.
   This way, sending tez to a user account works in the same way as sending tez to a smart contract.

   The `Tezos.Operation.transaction` function creates a Tezos transaction.
   There are many kinds of internal transactions in Tezos, but most smart contracts deal with these transactions:

      - Transferring tez to another account
      - Calling an entrypoint on a smart contract

   Calling an entrypoint on a smart contract (either the current contract or another contract) is beyond the scope of this tutorial.
   For information, see [Calling a contract](../../syntax/contracts/operation#calling-a-contract).

   The `Tezos.Operation.transaction` function takes these parameters:

   1. The parameter to pass, in this case `unit`, which means no value
   1. The amount of tez to include with the transaction, in this case all of the tez the contract has, denoted by the `Tezos.get_balance` function
   1. The address of the target contract

1. Add this code to calculate the new value of the storage, using the existing admin address and the default taco data:

   ```cameligo skip
   (* Restore stock of tacos *)
   let new_storage : storage = {
     admin_address = storage.admin_address;
     taco_data = default_taco_data
   } in
   ```

1. Replace the last line of the `payout` entrypoint with this code:

   ```cameligo skip
   [payout_operation], new_storage
   ```

   Creating the transaction is not enough to run it; you must return it in the list of operations at the end of the entrypoint.

The complete entrypoint looks like this:

```cameligo skip
[@entry]
let payout (_u : unit) (storage : storage) : operation list * storage =

  (* Ensure that only the admin can call this entrypoint *)
  let _ = if (Tezos.get_sender () <> storage.admin_address) then
    failwith "Only the admin can call this entrypoint" in

  (* Create contract object that represents the target account *)
  let receiver_contract = match Tezos.get_contract_opt storage.admin_address with
  | Some contract -> contract
  | None -> failwith "Couldn't find account" in

  (* Create operation to send tez *)
  let payout_operation = Tezos.Operation.transaction unit (Tezos.get_balance ()) receiver_contract in

  (* Restore stock of tacos *)
  let new_storage : storage = {
    admin_address = storage.admin_address;
    taco_data = default_taco_data
  } in

  [payout_operation], new_storage
```

</Syntax>

That's all you need to do to reset the storage and send the contract's tez to the administrator.
If you want to extend this logic, try separating the `payout` entrypoint into separate entrypoints for paying out the tez and resetting the stock of tacos.

## Testing the new entrypoint

Of course, after you implement the `payout` entrypoint, you should add tests for it.

<Syntax syntax="jsligo">

1. At the end of the test function, add this code to get the current balance of Pedro's account before calling the entrypoint:

   ```jsligo skip
   // Test the payout entrypoint as the administrator
   const admin_balance_before = Test.Address.get_balance(admin_address);
   ```

1. Add this code to set the account that smart contract calls come from in the test scenario:

   ```jsligo skip
   Test.State.set_source(admin_address);
   ```

   Now when you call the `Test.Contract.transfer` function, the transaction comes from Pedro's account.

1. Add this code to call the `payout` entrypoint and verify that the storage was updated, as in previous tests:

   ```jsligo skip
   const payout_result =
     Test.Contract.transfer(
       Test.Typed_address.get_entrypoint("payout", contract.taddr),
       unit,
       0 as tez
     );
   $match(payout_result, {
     "Success": (_s) => (() => {
         const storage = Test.Typed_address.get_storage(contract.taddr);
         // Check that the stock has been reset
         Assert.assert(
           eq_in_map(
             Map.find(1 as nat, TacoShop.default_taco_data),
             storage.taco_data,
             1 as nat
           ));
         Assert.assert(
           eq_in_map(
             Map.find(2 as nat, TacoShop.default_taco_data),
             storage.taco_data,
             2 as nat
           ));
         Test.IO.log("Successfully reset taco storage");
       })(),
       "Fail": (_err) => failwith("Failed to reset taco storage"),
     });
   ```

1. Add this code to verify that Pedro's account received the tez from the contract:

   ```jsligo skip
   // Check that the admin account got a payout
   const admin_balance_after = Test.Address.get_balance(admin_address);
   Assert.assert(Test.Compare.lt(admin_balance_before, admin_balance_after));
   ```

   The exact amounts differ because calling the `payout` entrypoint costs a small fee, but this code verifies that Pedro's account has more tez in it after calling the `payout` entrypoint.

1. Add this code to generate a test account and verify that it can't call the `payout` entrypoint because it is not the administrator:

   ```jsligo skip
   // Verify that the entrypoint fails if called by someone else
   const other_user_account = Test.Account.address(1 as nat);
   Test.State.set_source(other_user_account);
   const failed_payout_result =
     Test.Contract.transfer(
       Test.Typed_address.get_entrypoint("payout", contract.taddr),
       unit,
       0 as tez
     );
   $match(failed_payout_result, {
     "Success": (_s) => failwith("A non-admin user was able to call the payout entrypoint"),
     "Fail": (_err) => Test.IO.log("Successfully prevented a non-admin user from calling the payout entrypoint"),
   });
   ```

1. Run the test with `ligo run test taco_shop.jsligo` and verify that the test runs successfully.

The complete contract and tests looks like this:

```jsligo group=getting_payouts
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

    // Ensure that only the admin can call this entrypoint
    if (Tezos.get_sender() != storage.admin_address) {
      failwith("Only the admin can call this entrypoint");
    }

    // Create contract object that represents the target account
    const receiver_contract = $match(Tezos.get_contract_opt(storage.admin_address), {
      "Some": (contract) => contract,
      "None": () => failwith("Couldn't find account"),
    });

    // Create operation to send tez
    const payout_operation = Tezos.Operation.transaction(unit, Tezos.get_balance(), receiver_contract);

    // Restore stock of tacos
    const new_storage: storage = {
      admin_address: storage.admin_address,
      taco_data: default_taco_data,
    };

    return [[payout_operation], new_storage];
  }
};

// Convenience function to get current taco price
const get_taco_price = (untyped_address: address, taco_kind_index: nat): tez => {
  const view_result_option: option<tez> = Tezos.View.call("get_taco_price", taco_kind_index, untyped_address);
  return $match(view_result_option, {
    "Some": (cost_mutez) => cost_mutez,
    "None": () => Test.Assert.failwith("Couldn't get the price of the taco."),
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

  // Test the payout entrypoint as the administrator
  const admin_balance_before = Test.Address.get_balance(admin_address);
  Test.State.set_source(admin_address);
  const payout_result =
    Test.Contract.transfer(
      Test.Typed_address.get_entrypoint("payout", contract.taddr),
      unit,
      0 as tez
    );
  $match(payout_result, {
    "Success": (_s) => (() => {
        const storage = Test.Typed_address.get_storage(contract.taddr);
        // Check that the stock has been reset
        Assert.assert(
          eq_in_map(
            Map.find(1 as nat, TacoShop.default_taco_data),
            storage.taco_data,
            1 as nat
          ));
        Assert.assert(
          eq_in_map(
            Map.find(2 as nat, TacoShop.default_taco_data),
            storage.taco_data,
            2 as nat
          ));
        Test.IO.log("Successfully reset taco storage");
      })(),
      "Fail": (_err) => failwith("Failed to reset taco storage"),
    });

  // Check that the admin account got a payout
  const admin_balance_after = Test.Address.get_balance(admin_address);
  Assert.assert(Test.Compare.lt(admin_balance_before, admin_balance_after));

  // Verify that the entrypoint fails if called by someone else
  const other_user_account = Test.Account.address(1 as nat);
  Test.State.set_source(other_user_account);
  const failed_payout_result =
    Test.Contract.transfer(
      Test.Typed_address.get_entrypoint("payout", contract.taddr),
      unit,
      0 as tez
    );
  $match(failed_payout_result, {
    "Success": (_s) => failwith("A non-admin user was able to call the payout entrypoint"),
    "Fail": (_err) => Test.IO.log("Successfully prevented a non-admin user from calling the payout entrypoint"),
  });

}) ();
```

</Syntax>

<Syntax syntax="cameligo">

1. At the end of the test function, replace the last block with this code so the function can continue:

   ```cameligo skip
   let () = match fail_result with
   | Success _s -> failwith "Test was able to buy a taco for the wrong price"
   | Fail _err -> Test.IO.log "Contract successfully blocked purchase with incorrect price" in
   ```

1. Add this code to get the current balance of Pedro's account before calling the entrypoint:

   ```cameligo skip
   (* Test the payout entrypoint as the administrator *)
   let admin_balance_before = Test.Address.get_balance admin_address in
   ```

1. Add this code to set the account that smart contract calls come from in the test scenario:

   ```cameligo skip
   let () = Test.State.set_source admin_address in
   ```

   Now when you call the `Test.Contract.transfer` function, the transaction comes from Pedro's account.

1. Add this code to call the `payout` entrypoint and verify that the storage was updated, as in previous tests:

   ```cameligo skip
   let payout_result = Test.Contract.transfer
     (Test.Typed_address.get_entrypoint "payout" contract.taddr)
     unit
     0tez
   in
   let () = match payout_result with
   | Success _s -> let storage = Test.Typed_address.get_storage contract.taddr in
     let () = Assert.assert
       (eq_in_map (Map.find 1n TacoShop.default_taco_data)
       storage.taco_data
       1n) in
     let () = Assert.assert
       (eq_in_map (Map.find 2n TacoShop.default_taco_data)
       storage.taco_data
       2n) in
     Test.IO.log "Successfully reset taco storage"
   | Fail _err -> failwith "Failed to reset taco storage" in
   ```

1. Add this code to verify that Pedro's account received the tez from the contract:

   ```cameligo skip
   (* Check that the admin account got a payout *)
   let admin_balance_after = Test.Address.get_balance admin_address in
   let () = Assert.assert (Test.Compare.lt admin_balance_before admin_balance_after) in
   ```

   The exact amounts differ because calling the `payout` entrypoint costs a small fee, but this code verifies that Pedro's account has more tez in it after calling the `payout` entrypoint.

1. Add this code to generate a test account and verify that it can't call the `payout` entrypoint because it is not the administrator:

   ```cameligo skip
   (* Verify that the entrypoint fails if called by someone else *)
   let other_user_account = Test.Account.address 1n in
   let _ = Test.State.set_source other_user_account in
   let failed_payout_result = Test.Contract.transfer
     (Test.Typed_address.get_entrypoint "payout" contract.taddr)
     unit
     0tez
   in
   match failed_payout_result with
   | Success _s -> failwith "A non-admin user was able to call the payout entrypoint"
   | Fail _err -> Test.IO.log "Successfully prevented a non-admin user from calling the payout entrypoint"
   ```

1. Run the test with `ligo run test taco_shop.mligo` and verify that the test runs successfully.

The completed contract file with the convenience functions and test functions looks like this:

```cameligo group=getting_payouts
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

    (* Ensure that only the admin can call this entrypoint *)
    let _ = if (Tezos.get_sender () <> storage.admin_address) then
      failwith "Only the admin can call this entrypoint" in

    (* Create contract object that represents the target account *)
    let receiver_contract = match Tezos.get_contract_opt storage.admin_address with
    | Some contract -> contract
    | None -> failwith "Couldn't find account" in

    (* Create operation to send tez *)
    let payout_operation = Tezos.Operation.transaction unit (Tezos.get_balance ()) receiver_contract in

    (* Restore stock of tacos *)
    let new_storage : storage = {
      admin_address = storage.admin_address;
      taco_data = default_taco_data
    } in

    [payout_operation], new_storage

end

(* Convenience function to get current taco price *)
let get_taco_price (untyped_address : address) (taco_kind_index : nat) : tez =
  let view_result_option : tez option = Tezos.View.call
    "get_taco_price"
    taco_kind_index
    untyped_address in
  match view_result_option with
  | Some cost_mutez -> cost_mutez
  | None -> Test.Assert.failwith "Couldn't get the price of a taco"

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
  let () = match fail_result with
  | Success _s -> failwith "Test was able to buy a taco for the wrong price"
  | Fail _err -> Test.IO.log "Contract successfully blocked purchase with incorrect price" in

  (* Test the payout entrypoint as the administrator *)
  let admin_balance_before = Test.Address.get_balance admin_address in

  let () = Test.State.set_source admin_address in

  let payout_result = Test.Contract.transfer
    (Test.Typed_address.get_entrypoint "payout" contract.taddr)
    unit
    0tez
  in
  let () = match payout_result with
  | Success _s -> let storage = Test.Typed_address.get_storage contract.taddr in
    let () = Assert.assert
      (eq_in_map (Map.find 1n TacoShop.default_taco_data)
      storage.taco_data
      1n) in
    let () = Assert.assert
      (eq_in_map (Map.find 2n TacoShop.default_taco_data)
      storage.taco_data
      2n) in
    Test.IO.log "Successfully reset taco storage"
  | Fail _err -> failwith "Failed to reset taco storage" in

  (* Check that the admin account got a payout *)
  let admin_balance_after = Test.Address.get_balance admin_address in
  let () = Assert.assert (Test.Compare.lt admin_balance_before admin_balance_after) in

  (* Verify that the entrypoint fails if called by someone else *)
  let other_user_account = Test.Account.address 1n in
  let _ = Test.State.set_source other_user_account in
  let failed_payout_result = Test.Contract.transfer
    (Test.Typed_address.get_entrypoint "payout" contract.taddr)
    unit
    0tez
  in
  match failed_payout_result with
  | Success _s -> failwith "A non-admin user was able to call the payout entrypoint"
  | Fail _err -> Test.IO.log "Successfully prevented a non-admin user from calling the payout entrypoint"
```

</Syntax>

Now you can allow different users to do different things in the contract.

## Conclusion

Now you have a contract that Pedro can use to sell tacos and manage the profits and the taco stock.
From here you can expand the contract in many ways, such as:

- Adding more types of tacos
- Changing how the price of tacos is calculated
- Expanding the administrator functionality
- Accepting more than the price of the taco as a tip
- Adding more tests

You can also try deploying the contract to a test network and trying it in a real Tezos environment.
For a tutorial that covers deploying a contract, see [Deploy a smart contract](https://docs.tezos.com/tutorials/smart-contract) on docs.tezos.com.
