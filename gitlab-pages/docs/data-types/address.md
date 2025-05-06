---
id: address
title: Address
---

import Syntax from '@theme/Syntax';

An address on the Tezos blockchain is a string whose suffix is the
hash of the public key of an account.

- If the account is a smart contract (sometimes known as an *originated account*), the address starts with `KT1`.

- If the account is a user account (sometimes known as an *implicit account*), the address starts with `tz1`, `tz2`, `tz3`, or `tz4`.

For more information about Tezos addresses, see [Accounts and addresses](https://docs.tezos.com/architecture/accounts) on docs.tezos.com.

The `address` type represents a well-formed Tezos address.
However, the type being well-formed does not automatically mean that the address is valid and identifies a real account.
Beware of failures if the address is invalid.

To create an address in LIGO, cast a string to the `address` type, as in this example:

<Syntax syntax="cameligo">

```cameligo group=address
let my_account : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=address
const my_account: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;
```

</Syntax>

To get the address of the current contract, see [Contracts](../syntax/contracts).

## Sender

The *sender* is the address of the account (smart contract or user account) that initiated the current transaction.
The sender can be different from the *source* of the transaction.

For example, suppose that user account A calls contract B and as a result, contract B calls contract C.
In the context of contract C, contract B is the sender because it sent the immediate transaction to contract C.
Conversely, account A is the source because it initiated the current chain of transactions.
Account A is the source of all subsequent transactions in the chain, including any transactions that result from the call to contract C.

<Syntax syntax="cameligo">

```cameligo group=sender
module Tezos = Tezos.Next

let sender : address = Tezos.get_sender ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sender
import Tezos = Tezos.Next;

const sender: address = Tezos.get_sender();
```

</Syntax>

## Source

The *source* is the address of the user (implicit) account that initiated the current chain of transactions.
If there is more than one transaction in the chain, that address is different from the *sender*.

<Syntax syntax="cameligo">

```cameligo group=source
module Tezos = Tezos.Next
let source : address = Tezos.get_source ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=source
import Tezos = Tezos.Next;
const source: address = Tezos.get_source();
```

</Syntax>
