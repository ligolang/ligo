---
id: tezos-taco-shop-smart-contract
title: The Taco Shop Smart Contract
---

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
<div style="opacity: 0.7; text-align: center; font-size: 10px;">Made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/" 			    title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" 			    title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
</div>

---

## Pricing

Pedro's tacos are a rare delicacy, so their **price goes up** as the
**stock for the day begins to deplete**.

Each taco kind, has its own `max_price` that it sells for, and a
finite supply for the current sales lifecycle.

> For the sake of simplicity, we will not implement the replenishing
> of the supply after it has run out.

### Daily Offer

|**kind** |id |**available_stock**| **max_price**|
|---|---|---|---|
|Clásico | `1n` | `50n` | `50tez` |
|Especial del Chef | `2n` | `20n` | `75tez` |

### Calculating the Current Purchase Price

The current purchase price is calculated with the following formula:

```pascaligo skip
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

## Installing LIGO

In this tutorial, we will use LIGO's dockerized version, for the sake
of simplicity. You can find the installation instructions
[here](intro/installation.md#dockerized-installation-recommended).

The best way to install the dockerized LIGO is as a **global
executable** through the installation script, as shown in the
screenshot below:

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/install-ligo.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Installing the <b>next</b> version of LIGO's CLI</div>

## Implementing our First `main` Function

> From now on we will get a bit more technical. If you run into
> something we have not covered yet - please try checking out the
> [LIGO cheat sheet](api/cheat-sheet.md) for some extra tips & tricks.

To begin implementing our smart contract, we need a *main function*,
that is the first function being executed. We will call it `main` and
it will specify our contract's storage (`int`) and input parameter
(`int`). Of course this is not the final storage/parameter of our
contract, but it is something to get us started and test our LIGO
installation as well.

### `taco-shop.ligo`
```pascaligo group=a
function main (const parameter : int; const contractStorage : int) :
list (operation) * int is
  ((nil : list (operation)), contractStorage + parameter)
```

Let us break down the contract above to make sure we understand each
bit of the LIGO syntax:

- **`function main`** - definition of the main function, which takes
  a the parameter of the contract and the storage
- **`(const parameter : int;  const contractStorage : int)`** -
  parameters passed to the function: the first is called `parameter`
  because it denotes the parameter of a specific invocation of the
  contract, the second is the storage
- **`(list (operation) * int)`** - return type of our function, in our
  case a tuple with a list of operations, and an `int` (new value for
  the storage after a succesful run of the contract)
- **`((nil : list (operation)), contractStorage + parameter)`** -
  essentially a return statement
- **`(nil : list (operation))`**  - a `nil` value annotated as a list
  of operations, because that is required by our return type specified
  above
  - **`contractStorage + parameter`** - a new storage value for our
  contract, sum of previous storage and a transaction parameter

### Running LIGO for the First Time

To test that we have installed LIGO correctly, and that
`taco-shop.ligo` is a valid contract, we will dry-run it.

> Dry-running is a simulated execution of the smart contract, based on
> a mock storage value and a parameter.

Our contract has a storage of `int` and accepts a parameter that is
also an `int`.

The `dry-run` command requires a few parameters:
- **contract** *(file path)*
- **entrypoint** *(name of the main function in the contract)*
- **parameter** *(parameter to execute our contract with)*
- **storage** *(starting storage before our contract's code is executed)*

It outputs what is returned from our main function: in our case a
tuple containing an empty list (of operations to apply) and the new
storage value, which, in our case, is the sum of the previous storage
and the parameter we have used for the invocation.

```zsh
# Contract: taco-shop.ligo
# Main function: main
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

## Designing the Taco Shop's Contract Storage

We know that Pedro's Taco Shop serves two kinds of tacos, so we will
need to manage stock individually, per kind. Let us define a type,
that will keep the `stock` & `max_price` per kind in a record with two
fields. Additionally, we will want to combine our `taco_supply` type
into a map, consisting of the entire offer of Pedro's shop.

**Taco shop's storage**
```pascaligo group=b
type taco_supply is record [
  current_stock : nat;
  max_price     : tez
]

type taco_shop_storage is map (nat, taco_supply)
```

Next step is to update the `main` function to include
`taco_shop_storage` in its storage. In the meanwhile, let us set the
`parameter` to `unit` as well to clear things up.

**`taco-shop.ligo`**
```pascaligo group=b+
type taco_supply is record [
  current_stock : nat;
  max_price     : tez
]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

function main (const parameter : unit; const taco_shop_storage :  taco_shop_storage) : return is
  ((nil : list (operation)), taco_shop_storage)
```

### Populating our Storage in a dry-run

When dry-running a contract, it is crucial to provide a correct
initial storage value.  In our case the storage is type-checked as
`taco_shop_storage`. Reflecting
[Pedro's daily offer](tutorials/get-started/tezos-taco-shop-smart-contract.md#daily-offer),
our storage's value will be defined as follows:

**Storage value**
```zsh
map [
  1n -> record [
          current_stock = 50n;
          max_price = 50tez
        ];
  2n -> record [
          current_stock = 20n;
          max_price = 75tez
        ]
]
```

> The storage value is a map with two bindings (entries) distinguished
> by their keys `1n` and `2n`.

**Dry run command with a multi-line storage value**
```zsh
ligo dry-run taco-shop.ligo --syntax pascaligo main unit "map [
    1n -> record [
            current_stock = 50n;
            max_price = 50tez
          ];
    2n -> record [
            current_stock = 20n;
            max_price = 75tez
          ]
]"
```

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-2.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Dry-run with a complex storage value</div>

<br/>

*If everything went as expected, the `dry-run` command will return an
 empty list of operations and the contract's current storage, which is
 the map of the products we have defined based on the daily offer of
 Pedro's taco shop.*

---

## Providing another Access Function for Buying Tacos

Now that we have our stock well defined in form of storage, we can
move on to the actual sales. The `main` function will take a key `id`
from our `taco_shop_storage` map and will be renamed `buy_taco` for
more readability. This will allow us to calculate pricing, and if the
sale is successful, we will be able to reduce our stock because we
have sold a taco!

### Selling the Tacos for Free

Let is start by customizing our contract a bit, we will:

- rename `parameter` to `taco_kind_index`
- change `taco_shop_storage` to a `var` instead of a `const`, because
  we will want to modify it

**`taco-shop.ligo`**
```pascaligo group=c
type taco_supply is record [
    current_stock : nat;
    max_price : tez
]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

function buy_taco (const taco_kind_index : nat; var taco_shop_storage : taco_shop_storage) : return is
  ((nil : list (operation)), taco_shop_storage)
```

#### Decreasing `current_stock` when a Taco is Sold

In order to decrease the stock in our contract's storage for a
specific taco kind, a few things needs to happen:

- retrieve the `taco_kind` from our storage, based on the
  `taco_kind_index` provided;
- subtract the `taco_kind.current_stock` by `1n`;
- we can find the absolute value of the subtraction above by
  calling `abs` (otherwise we would be left with an `int`);
- update the storage, and return it.

**`taco-shop.ligo`**

```pascaligo group=d
type taco_supply is record [
  current_stock : nat;
  max_price : tez
]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

function buy_taco (const taco_kind_index : nat; var taco_shop_storage : taco_shop_storage) : return is
  block {
    // Retrieve the taco_kind from the contract's storage or fail
    const taco_kind : taco_supply =
      case taco_shop_storage[taco_kind_index] of
        Some (kind) -> kind
      | None -> (failwith ("Unknown kind of taco.") : taco_supply)
      end;

    // Decrease the stock by 1n, because we have just sold one
    taco_kind.current_stock := abs (taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind
  } with ((nil : list (operation)), taco_shop_storage)
```

<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-3.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Stock decreases after selling a taco</div>

<br/>

### Making Sure We Get Paid for Our Tacos

In order to make Pedro's taco shop profitable, he needs to stop giving
away tacos for free. When a contract is invoked via a transaction, an
amount of tezzies to be sent can be specified as well. This amount is
accessible within LIGO as `amount`.

To make sure we get paid, we will:

- calculate a `current_purchase_price` based on the
  [equation specified earlier](tutorials/get-started/tezos-taco-shop-smart-contract.md#calculating-the-current-purchase-price)
- check if the sent `amount` matches the `current_purchase_price`:
  - if not, then our contract will fail (`failwith`)
  - otherwise, stock for the given `taco_kind` will be decreased and
    the payment accepted

**`taco-shop.ligo`**
```pascaligo group=e
type taco_supply is record [
  current_stock : nat;
  max_price     : tez
]

type taco_shop_storage is map (nat, taco_supply)

type return is list (operation) * taco_shop_storage

function buy_taco (const taco_kind_index : nat ; var taco_shop_storage : taco_shop_storage) : return is
  block {
    // Retrieve the taco_kind from the contract's storage or fail
    const taco_kind : taco_supply =
      case taco_shop_storage[taco_kind_index] of
        Some (kind) -> kind
      | None -> (failwith ("Unknown kind of taco.") : taco_supply)
      end;

     const current_purchase_price : tez =
       taco_kind.max_price / taco_kind.current_stock;

    if amount =/= current_purchase_price then
      // We won't sell tacos if the amount is not correct
      failwith ("Sorry, the taco you are trying to purchase has a different price");
    else skip;

    // Decrease the stock by 1n, because we have just sold one
    taco_kind.current_stock := abs (taco_kind.current_stock - 1n);

    // Update the storage with the refreshed taco_kind
    taco_shop_storage[taco_kind_index] := taco_kind
  } with ((nil : list (operation)), taco_shop_storage)
```

In order to test the `amount` sent, we will use the `--amount` option
of `dry-run`:

```zsh
ligo dry-run taco-shop.ligo --syntax pascaligo --amount 1 buy_taco 1n "map [
    1n -> record [
            current_stock = 50n;
            max_price = 50tez
          ];
    2n -> record [
            current_stock = 20n;
            max_price = 75tez
          ]
]"
```

** Purchasing a Taco with 1tez **
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-4.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px; margin-top:-24px;">Stock decreases after selling a taco, if the right amount of tezzies is provided</div>

<br/>

**Attempting to Purchase a Taco with 0.7tez**
<img src="/img/tutorials/get-started/tezos-taco-shop-smart-contract/dry-run-5.png" />
<div style="opacity: 0.7; text-align: center; font-size: 12px;
margin-top:-24px;">Stock does not decrease after a purchase attempt
with an insufficient payment.</div>

<br/>

**That's it - Pedro can now sell tacos on-chain, thanks to Tezos & LIGO.**

---

## 💰 Bonus: *Accepting Tips above the Taco Purchase Price*

If you would like to accept tips in your contract, simply change the
following line, depending on your preference.

**Without tips**
```pascaligo skip
if amount =/= current_purchase_price then
```

**With tips**
```pascaligo skip
if amount >= current_purchase_price then
```
