---
id: addresses
title: Addresses
---

import Syntax from '@theme/Syntax';

An address on the Tezos blockchain is a string whose suffix is the
hash of the public key of a peer.

- If that peer is a smart contract, the address starts with `KT1`.

- If the peer is a user account, the address starts with `tz1`, `tz2`, `tz3`, or `tz4`.

The `address` type represents a well-formed Tezos address.
However, the type being well-formed does not automatically mean that the address is valid and identifies a real account.
Beware of failures if the address is invalid.

To create an address in LIGO, cast a string to the `address` type, as in these examples:

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