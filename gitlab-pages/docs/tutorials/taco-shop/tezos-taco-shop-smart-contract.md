---
id: tezos-taco-shop-smart-contract
title: The Taco Shop Smart Contract
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

<div>

Meet **Pedro**, our *artisan taco chef*, who has decided to open a
Taco shop on the Tezos blockchain, using a smart contract. He sells
two different kinds of tacos: **el Clásico** and the **Especial
del Chef**.

To help Pedro open his dream taco shop, we will implement a smart
contract that will manage supply, pricing & sales of his tacos to the
consumers.

<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/taco-stand.svg" width="50%" />
<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '10px' }}>Made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/"    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>

---

## Pricing

Pedro's tacos are a rare delicacy, so their **price goes up** as the
**stock for the day begins to deplete**.

Each taco kind, has its own `max_price` that it sells for, and a
finite supply for the current sales life-cycle.

> For the sake of simplicity, we will not implement the replenishing
> of the supply after it has run out.

### Daily Offer

|**kind** |id |**available_stock**| **max_price**|
|---|---|---|---|
|Clásico | `1n` | `50n` | `50tez` |
|Especial del Chef | `2n` | `20n` | `75tez` |

### Calculating the Current Purchase Price

The current purchase price is calculated with the following formula:

```cameligo skip
current_purchase_price = max_price / available_stock
```

#### El Clásico
|**available_stock**|**max_price**|**current_purchase_price**|
|---|---|---|
| `50n` | `50tez` | `1tez`|
| `20n` | `50tez` | `2.5tez` |
| `5n` | `50tez` | `10tez` |

#### Especial del chef
|**available_stock**|**max_price**|**current_purchase_price**|
|---|---|---|
| `20n` | `75tez` | `3.75tez` |
| `10n` | `75tez` | `7.5tez`|
| `5n` | `75tez` | `15tez` |

---
## Draft a first contract
### Designing the Taco Shop's Contract Storage

First think to do when you create a smart contract is
think about what gonna be stored onto it.
We know that Pedro's Taco Shop serves two kinds of tacos, so we will
need to manage stock individually, per kind. Let us define a type,
that will keep the `stock` & `max_price` per kind in a record with two
fields. Additionally, we will want to combine our `taco_supply` type
into a map, consisting of the entire offer of Pedro's shop.

**Taco shop's storage**

<Syntax syntax="cameligo">

```cameligo group=b
type taco_supply = { current_stock : nat ; max_price : tez }

type taco_shop_storage = (nat, taco_supply) map
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
type taco_supply = { current_stock : nat , max_price : tez };

type taco_shop_storage = map <nat, taco_supply>;
```

</Syntax>

Now that the storage is defined, let's interact with it.

### Selling the Tacos for Free

Create your first entrypoint `buy_taco` which is doing nothing for now :

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage) : operation list * taco_shop_storage  = [], taco_shop_storage

```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@entry
function buy_taco(taco_kind_index: nat, taco_shop_storage: taco_shop_storage): [
  list<operation>,
  taco_shop_storage
]  {
    return [list([]), taco_shop_storage]
  };
```

</Syntax>

It's already possible to compile your contract by running : 

<Syntax syntax="jsligo">

```
ligo compile contract taco_shop.jsligo
```

</Syntax>

<Syntax syntax="cameligo">

```
ligo compile contract taco_shop.mligo
```

</Syntax>

> To avoid warning at compilation, change `taco_kind_index` into `_taco_kind_index`, it'll tell to the compiler that this variable is authorized to not be used.


A good practice is to scope your contract into a [module](https://ligolang.org/docs/language-basics/modules?lang=cameligo).

<Syntax syntax="cameligo">

```cameligo group=b12
module TacoShop = struct
  type taco_supply =
    {
     current_stock : nat;
     max_price : tez
    }

  type taco_shop_storage = (nat, taco_supply) map

  [@entry]
  let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage) : operation list * taco_shop_storage =
    [], taco_shop_storage

end 
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b12

namespace TacoShop {
  type taco_supply = { current_stock: nat, max_price: tez };
  export type taco_shop_storage = map<nat, taco_supply>;
  
  @entry
  function buy_taco(taco_kind_index: nat, taco_shop_storage: taco_shop_storage): [
    list<operation>,
    taco_shop_storage
  ] {
    return [list([]), taco_shop_storage]
  };
};

```

> We export `taco_shop_storage` to be accessible outside the module/namespace on the next section.

</Syntax>

There is an impact onto the compilation, now you have to tell to the compiler which [module](https://ligolang.org/docs/language-basics/modules?lang=cameligo) it need to compile :

```
ligo compile contract taco_shop.mligo -m TacoShop
```

### Populating our Storage

When deploying contract, it is crucial to provide a correct
initial storage value.  In our case the storage is type-checked as
`taco_shop_storage`, because the default storage is not directly used in the code, 
we encourage to declare the type, if your storage mutate, your default_storage will be in error. 
Reflecting [Pedro's daily offer](tezos-taco-shop-smart-contract.md#daily-offer),
our storage's value will be defined as follows:

<Syntax syntax="cameligo">

```cameligo group=b12
let default_storage: TacoShop.taco_shop_storage  = Map.literal [
  (1n, { current_stock = 50n ; max_price = 50tez }) ;
  (2n, { current_stock = 20n ; max_price = 75tez }) ;
]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b12
const default_storage: TacoShop.taco_shop_storage = Map.literal (list([
  [1n, { current_stock : 50n, max_price : 50tez }],
  [2n, { current_stock : 20n, max_price : 75tez }]
]));
```

</Syntax>

> The storage value is a map with two bindings (entries) distinguished
> by their keys `1n` and `2n`.

Out of curiosity, let's try to use LIGO `compile storage` command compile this value down to Michelson.

<Syntax syntax="cameligo">

```zsh
ligo compile storage taco_shop.jsligo default_storage -m TacoShop
# Output:
#
# { Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo compile storage taco_shop.jsligo default_storage -m TacoShop
# Output:
#
# { Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }
```

</Syntax>

Our initial storage record is compiled to a Michelson map `{ Elt 1 (Pair 50 50000000) ; Elt 2 (Pair 20 75000000) }`
holding the `current_stock` and `max_prize` in as a pair.

---
## Implement some logic

### Decreasing `current_stock` when a Taco is Sold

In order to decrease the stock in our contract's storage for a
specific taco kind, a few things needs to happen:

- retrieve the `taco_kind` from our storage, based on the
  `taco_kind_index` provided;
- subtract the `taco_kind.current_stock` by `1n`;
- we can find the absolute value of the subtraction above by
  calling `abs` (otherwise we would be left with an `int`);
- update the storage, and return it.

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage) : operation list * taco_shop_storage =
  (* Retrieve the taco_kind from the contract's storage or fail *)
  let taco_kind =
    match Map.find_opt (taco_kind_index) taco_shop_storage with
    | Some k -> k
    | None -> failwith "Unknown kind of taco"
  in
  (* Update the storage decreasing the stock by 1n *)
  let taco_shop_storage = Map.update
    taco_kind_index
    (Some { taco_kind with current_stock = abs (taco_kind.current_stock - 1n) })
    taco_shop_storage
  in
  [], taco_shop_storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@entry
function buy_taco(taco_kind_index: nat, taco_shop_storage: taco_shop_storage): [
  list<operation>,
  taco_shop_storage
]  {

  /* Retrieve the taco_kind from the contracts storage or fail */
  const taco_kind: taco_supply =
  match (Map.find_opt (taco_kind_index, taco_shop_storage)) {
    when(Some(kind)): kind;
    when(None()): (failwith ("Unknown kind of taco"))
  };

  // Update the storage decreasing the stock by 1n
  const taco_shop_storage_updated = Map.update (
    taco_kind_index,
    (Some (({...taco_kind, current_stock : abs (taco_kind.current_stock - (1n)) }))),
    taco_shop_storage );
  return [list([]), taco_shop_storage_updated]
};
```

</Syntax>

### Making Sure We Get Paid for Our Tacos

In order to make Pedro's taco shop profitable, he needs to stop giving
away tacos for free. When a contract is invoked via a transaction, an
amount of tezzies to be sent can be specified as well. This amount is
accessible within LIGO as `Tezos.get_amount`.

To make sure we get paid, we will:

- calculate a `current_purchase_price` based on the
  [equation specified earlier](tezos-taco-shop-smart-contract.md#calculating-the-current-purchase-price)
- check if the sent amount matches the `current_purchase_price`:
  - if not, then our contract will fail (`failwith`)
  - otherwise, stock for the given `taco_kind` will be decreased and
    the payment accepted

<Syntax syntax="cameligo">

```cameligo skip
let buy_taco (taco_kind_index : nat) (taco_shop_storage : taco_shop_storage)
  : operation list * taco_shop_storage =
    (* Retrieve the taco_kind from the contract's storage or fail *)

    let taco_kind =
      match Map.find_opt (taco_kind_index) taco_shop_storage with
        Some k -> k
      | None -> failwith "Unknown kind of taco" in
    let current_purchase_price : tez =
      taco_kind.max_price / taco_kind.current_stock in
    (* We won't sell tacos if the amount is not correct *)

    let () =
      if (Tezos.get_amount ()) <> current_purchase_price
      then
        failwith
          "Sorry, the taco you are trying to purchase has a different price" in
    (* Update the storage decreasing the stock by 1n *)

    let taco_shop_storage =
      Map.update
        taco_kind_index
        (Some
           {taco_kind with current_stock = abs (taco_kind.current_stock - 1n)})
        taco_shop_storage in
    [], taco_shop_storage
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const buy_taco = (taco_kind_index: nat, taco_shop_storage: taco_shop_storage) : [
    list<operation>,
    taco_shop_storage
  ] => {
  /* Retrieve the taco_kind from the contracts storage or fail */
  const taco_kind : taco_supply =
    match (Map.find_opt (taco_kind_index, taco_shop_storage)) {
      when(Some(kind)): kind;
      when(None()): failwith ("Unknown kind of taco")
    };
  const current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock ;
  /* We won't sell tacos if the amount is not correct */
  if ((Tezos.get_amount ()) != current_purchase_price) {
    return failwith ("Sorry, the taco you are trying to purchase has a different price")
  } else {
    /* Update the storage decreasing the stock by 1n */
    let taco_shop_storage = Map.update (
      taco_kind_index,
      (Some (({...taco_kind, current_stock : abs (taco_kind.current_stock - 1n) }))),
      taco_shop_storage );
    return [list([]), taco_shop_storage]
  }
};
```

</Syntax>

Now let's test our function against a few inputs using the LIGO test framework.
For that, we will have another file in which will describe our test:

<Syntax syntax="cameligo">

```cameligo test-ligo group=test
#import "gitlab-pages/docs/tutorials/taco-shop/tezos-taco-shop-smart-contract.mligo" "C"

let assert_string_failure (res : test_exec_result) (expected : string) =
  let expected = Test.eval expected in
  match res with
  | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal actual expected)
  | Fail _ -> failwith "contract failed for an unknown reason"
  | Success _ -> failwith "bad price check"

let test =
  (* originate the contract with a initial storage *)
  let init_storage = Map.literal [
      (1n, { current_stock = 50n ; max_price = 50tez }) ;
      (2n, { current_stock = 20n ; max_price = 75tez }) ; ]
  in
  let { addr ; code = _; size = _ } = Test.originate (contract_of C.TacoShop) init_storage 0tez in

  (* Test inputs *)
  let clasico_kind : C.TacoShop parameter_of = Buy_taco 1n in
  let unknown_kind : C.TacoShop parameter_of = Buy_taco 3n in

  (* Auxiliary function for testing equality in maps *)
  let eq_in_map (r : C.TacoShop.taco_supply) (m : C.TacoShop.taco_shop_storage) (k : nat) =
    match Map.find_opt k m with
    | None -> false
    | Some v -> v.current_stock = r.current_stock && v.max_price = r.max_price in

  (* Purchasing a Taco with 1tez and checking that the stock has been updated *)
  let ok_case : test_exec_result = Test.transfer addr clasico_kind 1tez in
  let () = match ok_case with
    | Success _ ->
      let storage = Test.get_storage addr in
      assert ((eq_in_map { current_stock = 49n ; max_price = 50tez } storage 1n) &&
              (eq_in_map { current_stock = 20n ; max_price = 75tez } storage 2n))
    | Fail _ -> failwith ("ok test case failed")
  in

  (* Purchasing an unregistred Taco *)
  let nok_unknown_kind = Test.transfer addr unknown_kind 1tez in
  let () = assert_string_failure nok_unknown_kind "Unknown kind of taco" in

  (* Attempting to Purchase a Taco with 2tez *)
  let nok_wrong_price = Test.transfer addr clasico_kind 2tez in
  let () = assert_string_failure nok_wrong_price "Sorry, the taco you are trying to purchase has a different price" in
  ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo test-ligo group=test
#import "gitlab-pages/docs/tutorials/taco-shop/tezos-taco-shop-smart-contract.jsligo" "C"

function assert_string_failure (res: test_exec_result, expected: string) {
  const expected_bis = Test.eval(expected);
  match(res) {
    when (Fail(x)):
      match(x) {
        when (Rejected(y)):
          assert(Test.michelson_equal(y[0], expected_bis))
        when (Balance_too_low(_)):
          failwith("contract failed for an unknown reason")
        when (Other(_o)):
          failwith("contract failed for an unknown reason")
      }
    when (Success(_s)):
      failwith("bad price check")
  };
}

const test = (
  (_u: unit): unit => {
      /* Originate the contract with a initial storage */

      let init_storage =
        Map.literal(
          list(
            [
              [1n, { current_stock: 50n, max_price: 50000000mutez }],
              [2n, { current_stock: 20n, max_price: 75000000mutez }]
            ]
          )
        );
      const { addr , code , size } =
        Test.originate(contract_of(C.TacoShop), init_storage, 0mutez);

      /* Test inputs */

      const clasico_kind : parameter_of C.TacoShop = Buy_taco (1n);
      const unknown_kind : parameter_of C.TacoShop = Buy_taco (3n);
      /* Auxiliary function for testing equality in maps */

      const eq_in_map = (r: C.TacoShop.taco_supply, m: C.TacoShop.taco_shop_storage, k: nat) =>
        match(Map.find_opt(k, m)) {
          when (None):
            false
          when (Some(v)):
            v.current_stock == r.current_stock && v.max_price == r.max_price
        };
      /* Purchasing a Taco with 1tez and checking that the stock has been updated */

      const ok_case: test_exec_result =
        Test.transfer(
          addr,
          clasico_kind,
          1000000mutez
        );
      
        match(ok_case) {
          when (Success(_s)):
            do {
              let storage = Test.get_storage(addr);
              assert(
                eq_in_map(
                  { current_stock: 49n, max_price: 50000000mutez },
                  storage,
                  1n
                )
                && eq_in_map(
                     { current_stock: 20n, max_price: 75000000mutez },
                     storage,
                     2n
                   )
              );
            }
          when (Fail(_e)):
            failwith("ok test case failed")
        };
      /* Purchasing an unregistred Taco */

      const nok_unknown_kind =
        Test.transfer(
          addr,
          unknown_kind,
          1000000mutez
        );
      assert_string_failure(nok_unknown_kind, "Unknown kind of taco");
      /* Attempting to Purchase a Taco with 2tez */

      const nok_wrong_price =
        Test.transfer(
          addr,
          clasico_kind,
          2000000mutez
        );
      
        assert_string_failure(
          nok_wrong_price,
          "Sorry, the taco you are trying to purchase has a different price"
        );
      return unit
    }
  ) ();
```

</Syntax>

Let's break it down a little bit:
- we include the file corresponding to the smart contract we want to
  test;
- we define `assert_string_failure`, a function reading a transfer
  result and testing against a failure. It also compares the failing
  data - here, a string - to what we expect it to be;
- `test` is actually performing the tests: Originates the taco-shop
  contract; purchasing a Taco with 1tez and checking that the stock
  has been updated ; attempting to purchase a Taco with 2tez and
  trying to purchase an unregistered Taco. An auxiliary function to
  check equality of values on maps is defined.

> checkout the [reference page](../../reference/test.md) for a more detailed description of the Test API

Now it is time to use the LIGO command `test`. It will evaluate our
smart contract and print the result value of those entries that start
with `"test"`:

<Syntax syntax="cameligo">

```zsh
ligo run test gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-test.mligo
# Output:
#
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>

<Syntax syntax="jsligo">

```zsh
ligo run test gitlab-pages/docs/tutorials/get-started/tezos-taco-shop-test.jsligo
# Output:
#
# Everything at the top-level was executed.
# - test exited with value ().
```

</Syntax>


**The test passed ! That's it - Pedro can now sell tacos on-chain, thanks to Tezos & LIGO.**

---

## 💰 Bonus: *Accepting Tips above the Taco Purchase Price*

If you would like to accept tips in your contract, simply change the
following line, depending on your preference.

**Without tips**

<Syntax syntax="cameligo">

```cameligo skip
if (Tezos.get_amount ()) <> current_purchase_price then
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
if ((Tezos.get_amount ()) != current_purchase_price)
```

</Syntax>

**With tips**

<Syntax syntax="cameligo">

```cameligo skip
if (Tezos.get_amount ()) >= current_purchase_price then
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
if ((Tezos.get_amount ()) >= current_purchase_price)
```

</Syntax>
