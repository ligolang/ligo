---
id: tezos-taco-shop-smart-contract
title: Taco shop smart-contract
---

<div>

Meet **Pedro**, our *artisan taco chef* who has decided to open a Taco shop on the Tezos blockchain, using a smart-contract. He sells two different kinds of tacos, the **el clásico** and the **especial del chef**. 

To help Pedro open his dream taco shop, we'll implement a smart-contract, that will manage supply, pricing & sales of his tacos to the consumers.

<br/>
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/taco-stand.svg" width="50%" />
<div style="opacity: 0.7; text-align: center; font-size: 10px;">Made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/" 			    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" 			    title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>

---

## Pricing

Pedro's tacos are a rare delicatese, so their **price goes up**, as the **stock for the day begins to deplete**.

Each taco kind, has it's own `max_price` that it sells for, and a finite supply for the current sales lifecycle.

> For the sake of simplicity, we won't implement replenishing of the supply after it runs out.

### Daily offer

|**kind** |id |**available_stock**| **max_price**|
|---|---|---|---|
|el clásico | `1n` | `50n` | `50000000mtz` |
|especial del chef | `2n` | `20n` | `75000000mtz` |

### Calculating the current purchase price

Current purchase price is calculated with the following equation:

```
current_purchase_price = max_price / available_stock
```

#### El clásico
|**available_stock**|**max_price**|**current_purchase_price**|
|---|---|---|
| `50n` | `50000000mtz` | `1tz`|
| `20n` | `50000000mtz` | `2.5tz` |
| `5n` | `50000000mtz` | `10tz` |

#### Especial del chef
|**available_stock**|**max_price**|**current_purchase_price**|
|---|---|---|
| `20n` | `75000000mtz` | `3.75tz` |
| `10n` | `75000000mtz` | `7.5tz`|
| `5n` | `75000000mtz` | `15tz` |

---

## Installing LIGO

In this tutorial, we'll use LIGO's dockerized version for the sake of simplicity. You can find the installation instructions [here](setup/installation.md#dockerized-installation-recommended).

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/install-ligo.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Installing the <b>next</b> version of LIGO's CLI</div>

## Implementing our first entry point

> From now on we'll get a bit more technical. If you run into something we have not covered yet - please try checking out the [LIGO cheat sheet](language-basics/cheat-sheet.md) for some extra tips & tricks.

To begin implementing our smart contract, we need an entry point. We'll call it `main` and it'll specify our contract's storage (`int`) and input parameter (`int`). Of course this is not the final storage/parameter of our contract, but it's something to get us started and test our LIGO installation as well.

### `taco-shop.ligo`
```Pascal
function main (const parameter : int;  const contractStorage : int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)), contractStorage + parameter)
```

Let's brake down the contract above to make sure we understand each bit of the LIGO syntax:

- **`function main`** - definition of a function that serves as an entry point
- **`(const parameter : int;  const contractStorage : int)`** - parameters passed to the function
  - **`const parameter : int`** - parameter provided by a transaction that invokes our contract
  - **`const contractStorage : int`** - definition of our storage (`int`)
- **`(list(operation) * int)`** - return type of our function, in our case a touple with a list of operations, and an int
- **`block {skip}`** - our function has no body, so we instruct LIGO to `skip` it
- **`with ((nil : list(operation)), contractStorage + parameter)`** - essentially a return statement
  - **`(nil : list(operation))`**  - a `nil` value annotated as a list of operations, because that's required by our return type specified above
  - **`contractStorage + parameter`** - a new storage value for our contract, sum of previous storage and a transaction parameter
### Running LIGO for the first time

To test that we've installed LIGO correctly, and that `taco-shop.ligo` is a valid contract, we'll dry-run it.

> Dry-running is a simulated execution of the smart contract, based on a mock storage value and a parameter.

Our contract has a storage of `int` and accepts a parameter that is also an `int`. 

The `dry-run` command requires a few parameters:
- **contract** *(file path)*
- **entrypoint** *(name of the entrypoint function in the contract)*
- **parameter** *(parameter to execute our contract with)*
- **storage** *(starting storage before our contract's code is executed)*


And outputs what's returned from our entrypoint - in our case a touple containing an empty list (of operations to apply) and the new storage value - which in our case is the sum of the previous storage and the parameter we've used.

```zsh
# Contract: taco-shop.ligo
# Entry point: main
# Parameter: 4
# Storage: 3
ligo dry-run taco-shop.ligo --syntax pascaligo main 4 3
# tuple[   list[]
#          7
# ]
```

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-1.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Simulating contract execution with the CLI</div>

<br/>

*`3 + 4 = 7` yay! Our CLI & contract work as expected, we can move onto fulfilling Pedro's on-chain dream.*

---

## Designing Taco shop's contract storage

We know that Pedro's Taco Shop serves two kinds of tacos, so we'll need to manage stock individually, per kind. Let's define a type, that will keep the `stock` & `max_price` per kind - in a record with two fields. Additionally, we'll want to combine our `taco_supply` type into a map, consisting of the entire offer of Pedro's shop.

**Taco shop's storage**
```Pascal
type taco_supply is record
    current_stock : nat;
    max_price : tez;
end

type taco_shop_storage is map(nat, taco_supply);
```

Next step is to update the `main` entry point to include `taco_shop_storage` as it's storage - while doing that let's set the `parameter` to `unit` as well to clear things up.

**`taco-shop.ligo`**
```Pascal
type taco_supply is record
    current_stock : nat;
    max_price : tez;
end
type taco_shop_storage is map(nat, taco_supply);

function main (const parameter: unit ; const taco_shop_storage : taco_shop_storage) : (list(operation) * taco_shop_storage) is
  block {skip} with ((nil : list(operation)), taco_shop_storage)
```

### Populating our storage in a dry-run

When dry-running a contract, it's crucial to provide a correct initial storage value - in our case the storage is type-checked as `taco_shop_storage`. Reflecting [Pedro's daily offer](tutorials/get-started/tezos-taco-shop-smart-contract.md#daily-offer), our storage's value will be defined as following:

**Storage value**
```zsh
map
    1n -> record
        current_stock = 50n;
        max_price = 50000000mtz;
    end;
    2n -> record
        current_stock = 20n;
        max_price = 75000000mtz;
    end;
end
```

> Storage value is a map, with two items in it, both items are records identified by natural numbers `1n` & `2n`.

**Dry run command with a multi-line storage value**
```zsh
ligo dry-run taco-shop.ligo --syntax pascaligo main unit "map
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

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-2.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Dry-run with a complex storage value</div>

<br/>

*If everything went as expected, the `dry-run` command will return the contract's current storage, which is the map of products we've defined based on the daily offer of Pedro's taco shop.*

---

## Providing an entrypoint for buying tacos

Now that we have our stock well defined in form of storage, we can move on to the actual sales. We'll replace the `main` entrypoint with `buy_taco`, that takes an `id` - effectively a key from our `taco_shop_storage` map. This will allow us to calculate pricing, and if the sale is successful - then we can reduce our stock - because we have sold a taco!

### Selling the tacos for free

Let's start by customizing our contract a bit, we will:

- rename the entrypoint from `main` to `buy_taco`
- rename `parameter` to `taco_kind_index`
- change `taco_shop_storage` to a `var` instead of a `const`, because we'll want to modify it

**`taco-shop.ligo`**
```Pascal
type taco_supply is record
    current_stock : nat;
    max_price : tez;
end
type taco_shop_storage is map(nat, taco_supply);


function buy_taco (const taco_kind_index: nat ; var taco_shop_storage : taco_shop_storage) : (list(operation) * taco_shop_storage) is
  block { skip } with ((nil : list(operation)), taco_shop_storage)
```

#### Decreasing `current_stock` when a taco is sold

In order to decrease the stock in our contract's storage for a specific taco kind, a few things needs to happen:

- retrieve the `taco_kind` from our storage, based on the `taco_kind_index` provided
- subtract the `taco_kind.current_stock` by `1n`
  - we can find the absolute (`nat`) value of the subtraction above by using `abs`, otherwise we'd be left with an `int`
- update the storage, and return it

**`taco-shop.ligo`**

```Pascal
type taco_supply is record
    current_stock : nat;
    max_price : tez;
end
type taco_shop_storage is map(nat, taco_supply);

function buy_taco (const taco_kind_index: nat ; var taco_shop_storage : taco_shop_storage) : (list(operation) * taco_shop_storage) is
  begin
    // Retrieve the taco_kind from the contract's storage
    const taco_kind : taco_supply = get_force(taco_kind_index, taco_shop_storage);
    // Decrease the stock by 1n, because we've just sold one
    taco_kind.current_stock := abs(taco_kind.current_stock - 1n);
    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind;
  end with ((nil : list(operation)), taco_shop_storage)
```

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-3.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Stock decreases after selling a taco</div>

<br/>

### Making sure we get paid for our tacos

In order to make Pedro's taco shop profitable, he needs to stop giving away tacos for free. When a contract is invoked via a transaction, an amount of tezzies to be sent can be specified as well. This amount is accessible within LIGO as `amount`.

To make sure we get paid, we will:

- calculate a `current_purchase_price` based on the [equation specified earlier](tutorials/get-started/tezos-taco-shop-smart-contract.md#calculating-the-current-purchase-price)
- check if the sent `amount` matches the `current_purchase_price`
  - if not, then our contract will `fail` and stop executing
  - if yes, stock for the given `taco_kind` will be decreased and the payment accepted

**`taco-shop.ligo`**
```Pascal
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

In order to test the `amount` sent, we'll use the `--amount` option of `dry-run`:

```zsh
ligo dry-run taco-shop.ligo--syntax pascaligo --amount 1 buy_taco 1n "map
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
**Purchasing a taco with 1.0tz**
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-4.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Stock decreases after selling a taco, if the right amount of tezzies is provided</div>

<br/>

**Attempting to purchase a taco with 0.7tz**
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-5.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Stock does not decrease after a purchase attempt with a lower than required amount.</div>

<br/>

**That's it - Pedro can now sell tacos on-chain, thanks to Tezos & LIGO.**

---

## 💰 Bonus: *Accepting tips above the taco purchase price*

If you'd like to accept tips in your contract as well, simply change the following line, depending on which behavior do you prefer.

**Without tips**
```Pascal
if amount =/= current_purchase_price then
```

**With tips**
```Pascal
if amount >= current_purchase_price then
```
