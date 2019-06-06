---
id: version-next-language-basics-operators
title: Operators
original_id: language-basics-operators
---

## Available operators

|Michelson   	|Pascaligo   	|Description |
|---	|---	|---	|
| `SENDER` | `sender` | Address that initiated the current transaction
| `SOURCE` | `source` | Address that initiated the transaction, which triggered the current transaction. (useful e.g. when there's a transaction sent by another contract)
| `AMOUNT` | `amount` | Amount of tez sent by the transaction that invoked the contract