---
id: operators
title: Operators
---

## Available operators

> This list is non-exhaustive, more operators will be added in the upcoming LIGO releases.

|Michelson   	|Pascaligo   	|Description |
|---	|---	|---	|
| `SENDER` | `sender` | Address that initiated the current transaction
| `SOURCE` | `source` | Address that initiated the transaction, which triggered the current transaction. (useful e.g. when there's a transaction sent by another contract)
| `AMOUNT` | `amount` | Amount of tez sent by the transaction that invoked the contract
| `NOW`    | `now`    | Timestamp of the block whose validation triggered execution of the contract, i.e. current time when the contract is run.