---
id: operators
title: Operators
---

## Available Michelson Operators

> This list is non-exhaustive. More operators will be added in
> upcoming LIGO releases.

| Michelson | LIGO               | Description
|----------|--------------------|-------------
| `SENDER` | `Tezos.get_sender` | Address that initiated the current transaction
| `SOURCE` | `Tezos.get_source` | Address that initiated the transaction, which triggered the current transaction. (useful e.g. when there's a transaction sent by another contract)
| `AMOUNT` | `Tezos.get_amount` | Amount of tez sent by the transaction that invoked the contract
| `NOW`    | `Tezos.get_now`    | Timestamp of the block whose validation triggered execution of the contract, i.e. current time when the contract is run.

<!-- updated use of entry -->