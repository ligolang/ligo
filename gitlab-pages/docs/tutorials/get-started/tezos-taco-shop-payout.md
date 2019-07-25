---
id: tezos-taco-shop-payout
title: Paying out profits from the Taco Shop
---

In the [previous tutorial](tutorials/get-started/tezos-taco-shop-smart-contract.md) we've learned how to setup & interact with the LIGO CLI. Followed by implementation of a simple Taco Shop smart contract for our entepreneur Pedro. In this tutorial we'll make sure Pedro has access to tokens that people have spent at his shop when buying tacos.


<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-payout/get-money.svg" width="50%" />

<div style="opacity: 0.7; text-align: center; font-size: 10px;">
<div>Icons made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/"                 title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/"                 title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>


## Analyzing the current contract

### **`taco-shop.ligo`**
```
type taco_supply is record
    current_stock : nat;
    max_price : tez;
end
type taco_shop_storage is map(nat, taco_supply);

function buy_taco (const taco_kind_index: nat ; var taco_shop_storage : taco_shop_storage) : (list(operation) * taco_shop_storage) is
  begin
    // Retrieve the taco_kind from the contract's storage
    const taco_kind : taco_supply = get_force(taco_kind_index, taco_shop_storage);
    
    const current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock;

    if amount =/= current_purchase_price then
      // we won't sell tacos if the amount isn't correct
      fail("Sorry, the taco you're trying to purchase has a different price");
    else
      // Decrease the stock by 1n, because we've just sold one
      taco_kind.current_stock := abs(taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind;
  end with ((nil : list(operation)), taco_shop_storage)
```

### Purchase price formula
Pedro's Taco Shop contract currently enables customers to buy tacos, at a computed price based on a simple formula. 

```
const current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock;
```

### Replacing *spendable* smart contracts
However, due to the [recent protocol upgrade](http://tezos.gitlab.io/mainnet/protocols/004_Pt24m4xi.html) of the Tezos mainnet, Pedro can't access the tokens stored in his Shop's contract directly. This was previously possible via `spendable` smart contracts, which are no longer available in the new protocol. We will have to implement a solution to access tokens from the contract programatically.

---

## Designing a payout scheme

Pedro is a standalone bussines owner, and in our case, he doesn't have to split profits / earnings of the taco shop with anyone. So for the sake of simplicity, we'll payout all the earned XTZ directly to Pedro right after a succesful taco purchase.

This means that after all the *purchase conditions* of our contract are met - e.g. correct amount is sent to the contract - we'll not only decrease the supply of the individual purchased *taco kind*, but we'll also transfer this amount in a *subsequent transaction* to Pedro's personal address.

## Forging a payout transaction

### Defining the recipient
In order to send tokens, we will need a receiver address - which in our case will be Pedro's personal account. Additionally we'll wrap the given address as a *`contract(unit)`* - which represents either a contract with no parameters, or an implicit account.

```
const ownerAddress : address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
const receiver : contract(unit) = get_contract(ownerAddress);
```

> Would you like to learn more about addresses, contracts and operations in LIGO? Check out the [LIGO cheat sheet](language-basics/cheat-sheet.md)

### Adding the transaction to the list of output operations
Now we can transfer the `amount` received by `buy_taco` to Pedro's `ownerAddress`. We will do so by forging a `transaction(unit, amount, receiver)` within a list of operations returned at the end of our contract.


```
const payoutOperation : operation = transaction(unit, amount, receiver) ;
const operations : list(operation) = list
    payoutOperation
end;
```

---

## Finalizing the contract

### **`taco-shop.ligo`**
```
type taco_supply is record
    current_stock : nat;
    max_price : tez;
end
type taco_shop_storage is map(nat, taco_supply);

const ownerAddress: address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";

function buy_taco (const taco_kind_index: nat ; var taco_shop_storage : taco_shop_storage) : (list(operation) * taco_shop_storage) is
  begin
    // Retrieve the taco_kind from the contract's storage
    const taco_kind : taco_supply = get_force(taco_kind_index, taco_shop_storage);
    
    const current_purchase_price : tez = taco_kind.max_price / taco_kind.current_stock;

    if amount =/= current_purchase_price then
      // we won't sell tacos if the amount isn't correct
      fail("Sorry, the taco you're trying to purchase has a different price");
    else
      // Decrease the stock by 1n, because we've just sold one
      taco_kind.current_stock := abs(taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind;

    const receiver : contract(unit) = get_contract(ownerAddress);
    const payoutOperation : operation = transaction(unit, amount, receiver);
    const operations : list(operation) = list
      payoutOperation
    end;

  end with (operations, taco_shop_storage)
```


### Dry-run the contract

To confirm that our contract is valid, we can dry run it. As a result we see a *new operation* in the list of returned operations to be executed subsequently.

```
ligo dry-run taco-shop.ligo --syntax pascaligo --amount 1 buy_taco 1n "map
    1n -> record
        current_stock = 50n;
        max_price = 50000000mtz;
    end;
    2n -> record
        current_stock = 20n;
        max_price = 75000000mtz;
    end;
end"
```

<img src="/img/tutorials/get-started/tezos-taco-shop-payout/dry-run-1.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">
<b>Operation(...bytes)</b> included in the output
</div>

<br/>

**Done! Our tokens are no longer locked in the contract, and instead they are sent to Pedro's personal account/wallet.**

---

## 👼 Bonus: donating part of the profits

Because Pedro is a member of the (STA) Specialty Taco Association, he has decided to donate **10%** of the earnings to the STA. We'll just add a `donationAddress` to the contract, and compute a 10% donation sum from each taco purchase.

```
const ownerAddress: address = "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
const donationAddress: address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
```

```
const receiver : contract(unit) = get_contract(ownerAddress);
const donationReceiver : contract(unit) = get_contract(donationAddress);

const donationAmount: tez = amount / 10n;

const operations : list(operation) = list
  // Pedro will get 90% of the amount
  transaction(unit, amount - donationAmount, receiver);
  transaction(unit, donationAmount, donationReceiver);
end;
```

This will result into two operations being subsequently executed on the blockchain:
- Donation transfer (10%)
- Pedro's profits (90%)