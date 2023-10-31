---
id: tezos-taco-shop-payout
title: Paying out profits from the Taco Shop
---

import Syntax from '@theme/Syntax';

In the
[previous tutorial](tezos-taco-shop-smart-contract.md)
we have learnt how to setup & interact with the LIGO CLI. Followed an
implementation of a simple Taco Shop smart contract for our
entrepreneur Pedro.

In this tutorial we will make sure Pedro has access to tokens that
people have spent at his shop when buying tacos.

<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-payout/get-money.svg" width="50%" />

<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '10px' }}>
<div>Icons made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/"                 title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/"                 title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>


## Analysing the Current Contract

<Syntax syntax="cameligo">

### **`taco-shop.mligo`**

```cameligo group=a
module TacoShop = struct
  type taco_supply =
    {
     current_stock : nat;
     max_price : tez
    }

  type taco_shop_storage = (nat, taco_supply) map

  [@entry]
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

end

let default_storage : TacoShop.taco_shop_storage =
  Map.literal
    [
      (1n,
       {
        current_stock = 50n;
        max_price = 50000000mutez
       });
      (2n,
       {
        current_stock = 20n;
        max_price = 75000000mutez
       })
    ]
```

</Syntax>

<Syntax syntax="jsligo">

### **`taco-shop.jsligo`**

```jsligo group=a
namespace TacoShop {
  export type taco_supply = { current_stock: nat, max_price: tez };
  export type taco_shop_storage = map<nat, taco_supply>;

  @entry
  function buy_taco(taco_kind_index: nat, taco_shop_storage: taco_shop_storage): [
    list<operation>,
    taco_shop_storage
  ]  {
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
    const taco_shop_storage = Map.update (
      taco_kind_index,
      (Some ({...taco_kind, current_stock : abs (taco_kind.current_stock - 1n) })),
      taco_shop_storage );
    return [list([]), taco_shop_storage]
  }
  };
};

const default_storage: TacoShop.taco_shop_storage =
  Map.literal(
    list(
      [
        [1n, { current_stock: 50n, max_price: 50000000mutez }],
        [2n, { current_stock: 20n, max_price: 75000000mutez }]
      ]
    )
  );

```

</Syntax>

### Purchase Price Formula

Pedro's Taco Shop contract currently enables customers to buy tacos,
at a price based on a simple formula.

<Syntax syntax="cameligo">

```cameligo skip
let current_purchase_price : tez =
  taco_kind.max_price / taco_kind.current_stock
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const current_purchase_price : tez =
  taco_kind.max_price / taco_kind.current_stock
```

</Syntax>

---

## Designing a Payout Scheme

Pedro is a standalone business owner, and in our case, he does not
have to split profits and earnings of the taco shop with anyone. So
for the sake of simplicity, we will payout all the earned XTZ directly
to Pedro right after a successful purchase.

This means that after all the *purchase conditions* of our contract
are met, e.g., the correct amount is sent to the contract, we will not
only decrease the supply of the individual purchased *taco kind*, but
we will also transfer this amount in a *subsequent transaction* to
Pedro's personal address.

## Forging a Payout Transaction

### Defining the Recipient

In order to send tokens, we will need a receiver address, which, in
our case, will be Pedro's personal account. Additionally we will wrap
the given address as a *`contract (unit)`*, which represents either a
contract with no parameters, or an implicit account.

<Syntax syntax="cameligo">

```cameligo group=ex1
let ownerAddress : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address)
let receiver : unit contract =
  match (Tezos.get_contract_opt ownerAddress : unit contract option) with
    Some (contract) -> contract
  | None -> (failwith "Not a contract" : unit contract)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=ex1
const ownerAddress = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" as address)
const receiver : contract<unit> =
  match (Tezos.get_contract_opt(ownerAddress) as option<contract<unit>>) {
    when(Some(contract)): contract;
    when(None()): (failwith ("Not a contract") as contract<unit>)
  }
```

</Syntax>

> Would you like to learn more about addresses, contracts and
> operations in LIGO? Check out the
> [LIGO cheat sheet](../../api/cheat-sheet.md)

### Adding the Transaction to the List of Output Operations

Now we can transfer the amount received by `buy_taco` to Pedro's
`ownerAddress`. We will do so by forging a `transaction (unit, amount,
receiver)` within a list of operations returned at the end of our
contract.

<Syntax syntax="cameligo">

```cameligo group=ex1
let payoutOperation : operation = Tezos.transaction () (Tezos.get_amount ()) receiver
let operations : operation list = [payoutOperation]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=ex1
const payoutOperation : operation = Tezos.transaction (unit, Tezos.get_amount (), receiver) ;
const operations : list <operation> = list([payoutOperation]);
```

</Syntax>

---

## Finalising the Contract

<Syntax syntax="cameligo">

### **`taco-shop.mligo`**

```cameligo group=b
module TacoShop = struct
  type taco_supply =
    {
     current_stock : nat;
     max_price : tez
    }

  type taco_shop_storage = (nat, taco_supply) map

  let ownerAddress : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address)

  [@entry]
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

    let receiver : unit contract =
      match (Tezos.get_contract_opt ownerAddress : unit contract option) with
        Some (contract) -> contract
      | None -> (failwith "Not a contract" : unit contract) in 

    let payoutOperation : operation = Tezos.transaction () (Tezos.get_amount ()) receiver in
    let operations : operation list = [payoutOperation] in

    operations, taco_shop_storage
end

let default_storage : TacoShop.taco_shop_storage =
  Map.literal
    [
      (1n,
       {
        current_stock = 50n;
        max_price = 50000000mutez
       });
      (2n,
       {
        current_stock = 20n;
        max_price = 75000000mutez
       })
    ]
```

</Syntax>

<Syntax syntax="jsligo">

### **`taco-shop.jsligo`**

```jsligo group=b
namespace TacoShop {
  export type taco_supply = { current_stock: nat, max_price: tez };
  export type taco_shop_storage = map<nat, taco_supply>;

  const ownerAddress = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" as address);

  const donationAddress = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address);

  @entry
  function buy_taco(taco_kind_index: nat, taco_shop_storage: taco_shop_storage): [
    list<operation>,
    taco_shop_storage
  ]  {
    /* Retrieve the taco_kind from the contracts storage or fail */

    const taco_kind: taco_supply =
      match(Map.find_opt(taco_kind_index, taco_shop_storage)) {
        when (Some(kind)):
          kind
        when (None):
          failwith("Unknown kind of taco")
      };
    const current_purchase_price: tez =
      taco_kind.max_price / taco_kind.current_stock;
    /* We won't sell tacos if the amount is not correct */

    if ((Tezos.get_amount()) != current_purchase_price) {
      return failwith(
        "Sorry, the taco you are trying to purchase has a different price"
      )
    } else {
      /* Update the storage decreasing the stock by 1n */

      const taco_shop_storage =
        Map.update(
          taco_kind_index,
          (
            Some({
              ...taco_kind, current_stock: abs(taco_kind.current_stock - 1n)
            })
          ),
          taco_shop_storage
        );

      const receiver : contract<unit> =
      match ((Tezos.get_contract_opt (ownerAddress))) {
        when(Some(contract)): contract;
        when(None()): ((failwith ("Not a contract")))
      };

      const donationReceiver : contract<unit>  =
        match ((Tezos.get_contract_opt (donationAddress))) {
          when(Some(contract)): contract;
          when(None()): ((failwith ("Not a contract")))
        };

      const donationAmount = ((Tezos.get_amount ()) / 10n) as tez;

      // Pedro will get 90% of the amount
      const op1 = match ((Tezos.get_amount ()) - donationAmount) {
        when(Some(x)): Tezos.transaction (unit, x, receiver);
        when(None()): failwith ("Insufficient balance")
      };
      const op2 = Tezos.transaction (unit, donationAmount, donationReceiver);
      const operations : list<operation> = list([ op1 , op2 ]);

      return [operations, taco_shop_storage]
    }
  };
}
const default_storage: TacoShop.taco_shop_storage =
  Map.literal(
    list(
      [
        [1n, { current_stock: 50n, max_price: 50000000mutez }],
        [2n, { current_stock: 20n, max_price: 75000000mutez }]
      ]
    )
  );

```

</Syntax>

### Dry-run the Contract

To confirm that our contract is valid, we can dry-run it. As a result,
we see a *new operation* in the list of returned operations to be
executed subsequently.

<Syntax syntax="cameligo">

```cameligo skip
ligo run dry-run taco-shop.mligo --syntax cameligo --amount 1 --entry-point buy_taco 1n "Map.literal [
    (1n, {  current_stock = 50n;  max_price = 50tez }) ;
    (2n, {  current_stock = 20n;  max_price = 75tez }) ;
]"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
ligo run dry-run taco-shop.jsligo --syntax jsligo -m TacoShop --amount 1 --entry-point buy_taco '1n' "default_storage"
```

</Syntax>

<img src="/img/tutorials/get-started/tezos-taco-shop-payout/dry-run-1.png" />
<div style={{ opacity: 0.7, textAlign: 'center', fontSize: '12px', marginTop: '-24px' }}>
<b>Operation(...bytes)</b> included in the output
</div>

<br/>

**Done! Our tokens are no longer locked in the contract, and instead
  they are sent to Pedro's personal account/wallet.**

---

## 👼 Bonus: Donating Part of the Profits

Because Pedro is a member of the Speciality Taco Association (STA), he
has decided to donate **10%** of the earnings to the STA. We will just
add a `donationAddress` to the contract, and compute a 10% donation
sum from each taco purchase.

<Syntax syntax="cameligo">

```cameligo group=bonus
let ownerAddress : address = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address)
let donationAddress : address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)

let receiver : unit contract =
  match ((Tezos.get_contract_opt ownerAddress) : unit contract option) with
    Some contract -> contract
  | None -> ((failwith "Not a contract") : unit contract)

let donationReceiver : unit contract  =
  match ((Tezos.get_contract_opt donationAddress) : unit contract option) with
    Some contract -> contract
  | None -> ((failwith "Not a contract") : unit contract)

let donationAmount : tez = (Tezos.get_amount ()) / 10n

let operations : operation list =
    // Pedro will get 90% of the amount
    let op = match ((Tezos.get_amount ()) - donationAmount) with
      | Some x -> Tezos.transaction () x receiver
      | None -> (failwith "Insufficient balance")
    in
    [ op ; Tezos.transaction () donationAmount donationReceiver ]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bonus
const ownerAddress = ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" as address);
const donationAddress = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address);

const receiver : contract<unit> =
  match ((Tezos.get_contract_opt (ownerAddress)) as option<contract<unit>>) {
    when(Some(contract)): contract;
    when(None()): ((failwith ("Not a contract")) as contract<unit>)
  };

const donationReceiver : contract<unit>  =
  match ((Tezos.get_contract_opt (donationAddress)) as option<contract<unit>>) {
    when(Some(contract)): contract;
    when(None()): ((failwith ("Not a contract")) as contract<unit>)
  };

const donationAmount = ((Tezos.get_amount ()) / 10n) as tez;

// Pedro will get 90% of the amount
const op1 = match ((Tezos.get_amount ()) - donationAmount) {
  when(Some(x)): Tezos.transaction (unit, x, receiver);
  when(None()): failwith ("Insufficient balance")
};
const op2 = Tezos.transaction (unit, donationAmount, donationReceiver);
const operations : list<operation> = list([ op1 , op2 ]);
```

</Syntax>

This will result into two operations being subsequently executed on the blockchain:
- Donation transfer (10%)
- Pedro's profits (90%)
