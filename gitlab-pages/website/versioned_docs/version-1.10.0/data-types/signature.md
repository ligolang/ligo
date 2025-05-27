---
id: signature
title: Cryptographic signatures
---

import Syntax from '@theme/Syntax';

The `signature` type is used for Tezos signatures (edsig,
spsig). Signatures are created by casting a string. Beware of failure
if the signature is invalid.

Here is how you can define a signature:

<Syntax syntax="cameligo">

```cameligo group=signature
let my_sig : signature =
   ("edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" :
   signature)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=signature
const my_sig: signature =
"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7" as
signature;
```

</Syntax>

## Verifying signatures

Sometimes a contract needs to check that a message has been signed
by a particular key. For example, a point-of-sale system might want a
customer to sign a transaction so it can be processed
asynchronously. You can do this in LIGO using the `key` and
`signature` types.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is
> because that would require storing a private key on chain, which defeats the purposes of a private key.

This example shows how to verify that a message was signed by a specific public key:

<Syntax syntax="cameligo">

```cameligo group=signature
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=signature
const check_signature =
  (pk: key, signed: signature, msg: bytes) =>
  Crypto.check(pk, signed, msg);
```

</Syntax>
