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

You can get the address of the contract itself by calling `Tezos.get_self_address ()`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=address
const my_account: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;
```

You can get the address of the contract itself by calling `Tezos.get_self_address()`.

</Syntax>