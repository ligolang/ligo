---
title: "Part 1: Creating a contract"
pagination_prev: null
---

import Syntax from '@theme/Syntax';

<div>

Meet Pedro, our artisan taco chef, who has decided to open a Taco shop on the Tezos blockchain, using a smart contract.

In this tutorial, to help Pedro open his dream taco shop, you will implement a smart contract that manages supply, pricing, and sales of his tacos to the consumers.
This scenario is ideal for a smart contract because smart contracts behave much like vending machines: users send requests to them along with information and money.
If the request is correct, the smart contract does something in response, in this case giving the customer an imaginary taco.

<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/taco-stand.svg" width="50%" />
<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '10px' }}>Made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/"    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>

## Learning objectives

In this tutorial, you will learn how to:

- Set up a smart contract in JsLIGO or CameLIGO
- Define the storage for the contract
- Define what requests the contract can accept and how it behaves
- Implement the code that handles these requests
- Write tests that ensure that the contract behaves correctly

## Prerequisites

Before you begin, install LIGO as described in [Installation](../../intro/installation).

Optionally, you can also set up your editor to work with LIGO as described in [Editor Support](../../intro/editor-support).

## Syntaxes

LIGO has two syntaxes:

- JsLIGO is inspired by TypeScript/JavaScript, intended for web developers

- CameLIGO is inspired by OCaml, intended for functional programmers

The syntaxes do the same thing and have nearly all the same features, so which one you choose depends on your preference or programming background.
You can use either syntax for this tutorial, but you must use the same syntax for the entire contract.
Use the **Syntax Preference** slider at the top left of this page to select the syntax to use.

## Pricing

Pedro sells two kinds of tacos: **el Clásico** and the **Especial del Chef**.
His tacos are a rare delicacy and he has a finite amount of each kind, so the price goes up as the stock for the day depletes.
Taco prices are in tez, the currency of the Tezos blockchain.

The cost for one taco is the maximum price for the taco divided by the total number of tacos, as in this formula:

```
current_purchase_price = max_price / available_stock
```

For example, the maximum price for an el Clásico taco is 50 tez.
This table shows the price when there are certain amounts of tacos left:

| Number of tacos available | Maximum price | Purchase price |
|---|---|---|
| 50 | 50 tez | 1 tez |
| 20 | 50 tez | 2.5 tez |
| 5 | 50 tez | 10 tez |
| 1 | 50 tez | 50 tez |

The maximum price for an Especial del Chef taco is 75 tez, so the prices are different, as in this table:

| Number of tacos available | Maximum price | Purchase price |
|---|---|---|
| 20 | 75 tez | 3.75 tez |
| 10 | 75 tez | 7.5 tez|
| 5 | 75 tez | 15 tez |
| 1 | 75 tez | 75 tez |

## Setting up the data storage

Smart contracts can store persistent data.
Only the contract itself can write to its data, but the data is visible to outside users.
This data can be in many data types, including simple data types like numbers, Boolean values, and strings, and complex data types like arrays and maps.

Because the cost of a taco is determined by a formula, the contract needs to store only two pieces of data for each type of taco: the maximum price and the number of tacos currently in stock.
LIGO contracts store this type of data in a data type called a *map*, which is a key-value store where each key is the same data type and each value is the same data type.
Maps are flexible, so you can add and remove elements.

The key for this map is a natural number (also known as a *nat*, an integer zero or greater) and the value is a [Record](../../data-types/records) data type that has two fields: a natural number for the current stock of tacos and a tez amount for the maximum price.
In table format, the map data looks ike this:

Key | Value
--- | ---
1 | `{ current_stock: 50, maximum_price: 50tez }`
2 | `{ current_stock: 20, maximum_price: 75tez }`

Follow these steps to set up the data storage for your contract:

<Syntax syntax="jsligo">

1. Anywhere on your computer, create a folder to store your work for this tutorial with a name such as `TacoShopTutorial`.

1. In the folder, create a file named `taco_shop.jsligo` to store the code of the smart contract.
You can create and edit this file in any text editor.

1. In the file, create a type named `taco_supply` that represents the value of the map, consisting of a nat for the number of tacos and a tez value for the maximum price:

   ```jsligo skip
   export type taco_supply = { current_stock: nat, max_price: tez };
   ```

1. Create a map type named `taco_data`, with the key a nat and the value the `taco_supply` type:

   ```jsligo skip
   export type taco_data = map<nat, taco_supply>;
   ```

   This map can contain the supply and max price for any number of tacos, indexed by a natural number key.

1. Create an address type to store Pedro's account address, which allows him to lock some features of the contract behind an administrator account:

   ```jsligo skip
   export type admin_address = address;
   ```

1. Create a type to represent the storage for the contract.
In this case, the contract needs to store the taco data map and the administrator address, so the overall contract storage contains those two values:

   ```jsligo skip
   export type storage = {
     admin_address: admin_address,
     taco_data: taco_data,
   };
   ```

1. Create a constant to represent the starting values for the taco data map:

   ```jsligo skip
   export const default_taco_data: taco_data = Map.literal([
     [1 as nat, { current_stock: 50 as nat, max_price: 50 as tez }],
     [2 as nat, { current_stock: 20 as nat, max_price: 75 as tez }]
   ]);
   ```

   Note that the natural numbers are indicated with an `as nat` after the number; otherwise, LIGO assumes that numbers are integers.
   Similarly, the maximum prices of the tacos have `as tez` to indicate that they are amounts of tez.

1. To keep the code for the contract organized, put the types and values in a namespace named `TacoShop`.
The contract looks like this so far:

   ```jsligo skip
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

   };
   ```

</Syntax>

<Syntax syntax="cameligo">

1. Anywhere on your computer, create a folder to store your work for this tutorial with a name such as `TacoShopTutorial`.

1. In the folder, create a file named `taco_shop.mligo` to store the code of the smart contract.
You can create and edit this file in any text editor.

1. In the file, create a type named `taco_supply` that represents the value of the map, consisting of a nat for the number of tacos and a tez value for the maximum price:

   ```cameligo skip
   type taco_supply = { current_stock: nat; max_price: tez }
   ```

1. Create a map type named `taco_data`, with the key a nat and the value the `taco_supply` type:

   ```cameligo skip
   type taco_data = (nat, taco_supply) map
   ```

   This map can contain the supply and max price for any number of tacos, indexed by a natural number key.

1. Create an address type to store Pedro's account address, which allows him to lock some features of the contract behind an administrator account:

   ```cameligo skip
   type admin_address = address
   ```

1. Create a type to represent the storage for the contract.
In this case, the contract needs to store the taco data map and the administrator address, so the overall contract storage contains those two values:

   ```cameligo skip
   type storage = {
     admin_address: admin_address;
     taco_data: taco_data;
   }
   ```

1. Create a variable to represent the starting values for the taco data map:

   ```cameligo skip
   let default_taco_data: taco_data = Map.literal [
     (1n, { current_stock = 50n; max_price = 50tez });
     (2n, { current_stock = 20n; max_price = 75tez });
   ]
   ```

   Note that the natural numbers are indicated with an `n` after the number; otherwise, LIGO assumes that numbers are integers.
   Similarly, the maximum prices of the tacos are suffixed with `tez` to indicate that they are amounts of tez.

1. To keep the code for the contract organized, put the types and values in a module named `TacoShop`.
The contract looks like this so far:

   ```cameligo skip
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

   end
   ```

</Syntax>

## Getting the price of tacos

Because the price of tacos changes, it'll be helpful to have a function to get the current price of a certain kind of taco.

<Syntax syntax="jsligo">

Add this function inside the namespace, immediately after the `default_taco_data` constant:

```jsligo skip
// Internal function to get the price of a taco
const get_taco_price_internal = (taco_kind_index: nat, taco_data: taco_data): tez => {
  const taco_kind: taco_supply =
    $match (Map.find_opt(taco_kind_index, taco_data), {
      "Some": (kind) => kind,
      "None": () => failwith("Unknown kind of taco"),
    });
  return taco_kind.max_price / taco_kind.current_stock;
}
```

</Syntax>

<Syntax syntax="cameligo">

Add this function inside the module, immediately after the `default_taco_data` variable:

```cameligo skip
(* Internal function to get the price of a taco *)
let get_taco_price_internal (taco_kind_index : nat) (taco_data : taco_data) : tez =
  let taco_kind : taco_supply =
    match Map.find_opt taco_kind_index taco_data with
    | Some kind -> kind
    | None -> failwith "Unknown kind of taco"
    in
    taco_kind.max_price / taco_kind.current_stock
```

</Syntax>

This code uses the `Map.find_opt` function to get an entry from a map based on a key.
It returns an [option](../../data-types/variants#options) value, which is a data type that LIGO uses to handle cases where a value may not exist.
In this case, the option has the value for that key if the key exists or a `None` value if the key does not exist.
If the `taco_kind_index` parameter is not a valid taco ID, the transaction fails.

This is an internal function, so external callers can't call it directly.
Later, you will add a way for external callers to get the current price of a taco.

## Selling tacos

Contracts have one or more _entrypoints_, which are a kind of function that clients can call, like endpoints in an API or functions or methods in many other programming languages.
A contract can have any number of internal functions, but only the functions designated as entrypoints can be called by outside consumers and other contracts.

The contract you create in this tutorial has two entrypoints:

- An entrypoint named `buy_taco` which accepts the type of taco to buy and the price of the taco and deducts that type of taco from the current stock in storage
- An entrypoint named `payout` that sends the tez in the contract to Pedro and restocks the supply of tacos

As described in [Entrypoints](../../syntax/contracts/entrypoints), entrypoints must follow a specific signature to be compiled as entrypoints:

<Syntax syntax="jsligo">

- Entrypoints are functions marked with the `@entry` decorator, which (when used in a namespace) must be in a comment immediately before the function
- Entrypoints receive a parameter from the caller and the current state of the contract storage
- Entrypoints return a tuple consisting of a list of operations to run (such as calls to other smart contracts or transfers of tez) and the new state of the contract storage

1. In the smart contract file, within the `TacoShop` namespace, add this stub of an entrypoint:

   ```jsligo skip
   // Buy a taco
   // @entry
   const buy_taco = (taco_kind_index: nat, storage: storage): [
       list<operation>,
       storage
     ] => {

       // Entrypoint logic goes here

       return [[], updated_storage];
     }
   ```

   Your IDE may show an error that the `updated_storage` value is not defined, but you can ignore this error for now because you will define it in the next few steps.

   To call this entrypoint, the caller passes a nat to indicate the type of taco.
   The function automatically receives the current state of the storage as the last parameter.
   The line `return [[], updated_storage];` returns an empty list of operations to run and the new state of the storage.
   In the next few steps, you add logic to verify that the caller sent the correct price and to deduct the taco from the current stock.

1. Within the entrypoint, add code to get the admin address and the taco data by destructuring the storage parameter:

   ```jsligo skip
   const { admin_address, taco_data } = storage;
   ```

1. After this code, add code to get the type of taco that the caller requested based on the `taco_kind_index` parameter:

   ```jsligo skip
   // Retrieve the kind of taco from the contracts storage or fail
   const taco_kind: taco_supply =
     $match (Map.find_opt(taco_kind_index, taco_data), {
       "Some": (kind) => kind,
       "None": () => failwith("Unknown kind of taco"),
   });
   ```

1. After the code you just added, add this code to get the current price of a taco:

   ```jsligo skip
   // Get the current price of this type of taco
   const current_purchase_price = get_taco_price_internal(taco_kind_index, taco_data);
   ```

1. Add this code to verify that the caller sent the correct amount of tez with the transaction.
It uses the `Tezos.get_amount()` function, which returns the amount of tez that the caller sent:

   ```jsligo skip
   // Verify that the caller sent the correct amount of tez
   if ((Tezos.get_amount()) != current_purchase_price) {
     return failwith("Sorry, the taco you are trying to purchase has a different price");
   }
   ```

1. Add this code to verify that there is at least one taco in stock:

   ```jsligo skip
   // Verify that there is at least one of this type of taco
   if (taco_kind.current_stock == 0 as nat) {
     return failwith("Sorry, we are out of this type of taco");
   }
   ```

1. Add this code to calculate the updated taco data map and put it in the `updated_taco_data` constant:

   ```jsligo skip
   // Update the storage with the new quantity of tacos
   const updated_taco_data: taco_data = Map.update(
     taco_kind_index,
     ["Some" as "Some", {...taco_kind, current_stock: abs(taco_kind.current_stock - 1) }],
     taco_data);
   ```

   This code uses the `Map.update` function to create a new version of the map with an updated record.
   In this case, the new map updates the stock of the specified type of taco to be one less.
   It uses the `abs` function to ensure that the new stock of tacos is a nat, because subtraction yields an integer.

1. Create the new value of the contract storage, including the admin address and the updated taco data:

   ```jsligo skip
   const updated_storage: storage = {
     admin_address: admin_address,
     taco_data: updated_taco_data,
   };
   ```

   The next line is the line `return [[], updated_taco_data];`, which you added when you stubbed in the entrypoint code earlier.

   Now the `buy_taco` entrypoint updates the stock in storage to indicate that it has one less of that type of taco.
   The contract automatically accepts the tez that is included with the transaction.

1. After the code for the `buy_taco` entrypoint, stub in the code for the entrypoint that allows Pedro to retrieve the tez in the contract, which you will add in a later section:

   ```jsligo skip
   // @entry
   const payout = (_u: unit, storage: storage): [
       list<operation>,
       storage
     ] => {

     // Entrypoint logic goes here

     return [[], storage];
   }
   ```

   Currently this entrypoint does nothing, but you will add code for it later.

</Syntax>

<Syntax syntax="cameligo">

- Entrypoints are functions marked with the `@entry` attribute
- Entrypoints receive a parameter from the caller and the current state of the contract storage
- Entrypoints return a tuple consisting of a list of operations to run (such as calls to other smart contracts or transfers of tez) and the new state of the contract storage

1. In the smart contract file, within the `TacoShop` module, add this stub of an entrypoint:

   ```cameligo skip
   (* Buy a taco *)
   [@entry]
   let buy_taco (taco_kind_index : nat) (storage : storage) : operation list * storage =

       (* Entrypoint logic goes here *)

     [], updated_storage
   ```

   Your IDE may show an error that the `updated_storage` value is not defined, but you can ignore this error for now because you will define it in the next few steps.

   To call this entrypoint, the caller passes a nat to indicate the type of taco.
   The function automatically receives the current state of the storage as the last parameter.
   The line `[], updated_storage` returns an empty list of operations to run and the new state of the storage.
   In the next few steps, you add logic to verify that the caller sent the correct price and to deduct the taco from the current stock.

1. Within the entrypoint, add code to get the admin address and the taco data by destructuring the storage parameter:

   ```cameligo skip
   let { admin_address; taco_data } = storage in
   ```

1. After this code, add code to get the type of taco that the caller requested based on the `taco_kind_index` parameter:

   ```cameligo skip
   (* Retrieve the kind of taco from the contracts storage or fail *)
   let taco_kind : taco_supply =
     match Map.find_opt taco_kind_index taco_data with
     | Some kind -> kind
     | None -> failwith "Unknown kind of taco" in
   ```

1. After the code you just added, add this code to get the current price of a taco:

   ```cameligo skip
   (* Get the current price of this type of taco *)
   let current_purchase_price = get_taco_price_internal taco_kind_index taco_data in
   ```

1. Add this code to verify that the caller sent the correct amount of tez with the transaction.
It uses the `Tezos.get_amount()` function, which returns the amount of tez that the caller sent:

   ```cameligo skip
   (* Verify that the caller sent the correct amount of tez *)
   let _ = if (Tezos.get_amount () <> current_purchase_price) then
     failwith "Sorry, the taco you are trying to purchase has a different price" in
   ```

1. Add this code to verify that there is at least one taco in stock:

   ```cameligo skip
   (* Verify that there is at least one of this type of taco *)
   let _ = if (taco_kind.current_stock = 0n) then
     failwith "Sorry, we are out of this type of taco" in
   ```

1. Add this code to calculate the updated taco data map and put it in the `updated_taco_data` variable:

   ```cameligo skip
   (* Update the storage with the new quantity of tacos *)
   let updated_taco_data : taco_data = Map.update
     taco_kind_index
     (Some { taco_kind with current_stock = abs (taco_kind.current_stock - 1n) })
     taco_data in
   ```

   This code uses the `Map.update` function to create a new version of the map with an updated record.
   In this case, the new map updates the stock of the specified type of taco to be one less.
   It uses the `abs` function to ensure that the new stock of tacos is a nat, because subtraction yields an integer.

1. Create the new value of the contract storage, including the admin address and the updated taco data:

   ```cameligo skip
   let updated_storage : storage = {
     admin_address = admin_address;
     taco_data = updated_taco_data;
   } in
   ```

   The next line is the line `[], updated_taco_data`, which you added when you stubbed in the entrypoint code earlier.

   Now the `buy_taco` entrypoint updates the stock in storage to indicate that it has one less of that type of taco.
   The contract automatically accepts the tez that is included with the transaction.

1. After the code for the `buy_taco` entrypoint, stub in the code for the entrypoint that allows Pedro to retrieve the tez in the contract, which you will add in a later section:

   ```cameligo skip
   [@entry]
   let payout (_u : unit) (storage : storage) : operation list * storage =

     (* Entrypoint logic goes here *)

     [], storage
   ```

   Currently this entrypoint does nothing, but you will add code for it later.

</Syntax>

## Providing information to clients

Earlier, you added an internal function that calculated the price of a taco.
External clients can't call this function because it is private to the contract.

The contract should give Pedro's customers a way to get the current price of a taco.
However, because entrypoints don't return a value directly to the caller, an entrypoint isn't the best way to provide information to clients.

If you need to provide information to clients, one way is to use a _view_, which is a static function that returns a value to clients but does not change the storage or generate any operations.
Like entrypoints, views are functions that receive one or more parameters from the caller and the current value of the storage.
Unlike entrypoints, they return a single value to the caller instead of a list of operations and the new value of the storage.

<Syntax syntax="jsligo">

Add this view to the contract, after the `get_taco_price_internal` function and somewhere within the namespace:

```jsligo skip
// @view
const get_taco_price = (taco_kind_index: nat, storage: storage): tez =>
  get_taco_price_internal(taco_kind_index, storage.taco_data);
```

This view is merely a wrapper around the `get_taco_price_internal` function, but the `@view` decorator makes external clients able to call it.

For more information about views, see [Views](../../syntax/contracts/views).

The complete contract file looks like this:

```jsligo group=selling_tacos
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
```

</Syntax>

<Syntax syntax="cameligo">

Add this view to the contract, after the `get_taco_price_internal` function and somewhere within the module:

```cameligo skip
[@view]
let get_taco_price (taco_kind_index : nat) (storage : storage) : tez =
  get_taco_price_internal taco_kind_index storage.taco_data
```

This view is merely a wrapper around the `get_taco_price_internal` function, but the `@view` attribute makes external clients able to call it.

For more information about views, see [Views](../../syntax/contracts/views).

The complete contract file looks like this:

```cameligo group=selling_tacos
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
```

</Syntax>

## Compiling the contract

Before you can deploy the contract to Tezos, you must compile it to Michelson,the low-level language of contracts on Tezos.

Run this command to compile the contract:

<Syntax syntax="jsligo">

```bash
ligo compile contract -m TacoShop -o taco_shop.tz taco_shop.jsligo
```

</Syntax>

<Syntax syntax="cameligo">

```bash
ligo compile contract -m TacoShop -o taco_shop.tz taco_shop.mligo
```

</Syntax>

If compilation is successful, LIGO prints nothing to the console and writes the compiled contract to the file `taco_shop.tz`.
You don't need to interact with this file directly.

If you see errors, make sure your code matches the code in the previous section.

You now have a basic contract that can accept requests to sell tacos.
However, before you deploy it, you should test the contract to make sure it works.
Continue to [Part 2: Testing the contract](./testing-contract).
