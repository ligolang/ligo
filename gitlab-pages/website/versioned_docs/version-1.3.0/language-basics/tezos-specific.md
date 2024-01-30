---
id: tezos-specific
title: Tezos Domain-Specific Operations
---

import Syntax from '@theme/Syntax';

LIGO is a programming language for writing Tezos smart contracts. It
would be a little odd if it did not have any Tezos specific
functions. This page will tell you about them.

## Pack and Unpack

As Michelson provides the `PACK` and `UNPACK` instructions for data
serialisation, so does LIGO with `Bytes.pack` and `Bytes.unpack`.  The
former serialises Michelson data structures into a binary format, and
the latter reverses that transformation. Unpacking may fail, so the
return type of `Byte.unpack` is an option that needs to be annotated.

> ⚠️ `PACK` and `UNPACK` are Michelson instructions that are intended
> to be used by people that really know what they are doing. There are
> several risks and failure cases, such as unpacking a lambda from an
> untrusted source or casting the result to the wrong type. Do not use
> the corresponding LIGO functions without doing your homework first.


<Syntax syntax="cameligo">

```cameligo group=a
let id_string (p : string) : string option =
  let packed: bytes = Bytes.pack p in
  Bytes.unpack packed
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
const id_string = (p) => {
  let packed = Bytes.pack(p);
  return Bytes.unpack(packed);
};
```

</Syntax>


## Hashing Keys

It is often desirable to hash a public key. In Michelson, certain data
structures such as maps will not allow the use of the `key` type. Even
if this were not the case, hashes are much smaller than keys, and
storage on blockchains comes at a cost premium. You can hash keys with
a predefined functions returning a value of type `key_hash`.


<Syntax syntax="cameligo">

```cameligo group=b
let check_hash_key (kh1, k2 : key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in
  (kh1 = kh2), kh2
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const check_hash_key = (kh1: key_hash, k2: key) => {
  let kh2 = Crypto.hash_key(k2);
  return [kh1 == kh2, kh2];
};
```

</Syntax>


## Checking Signatures

Sometimes a contract will want to check that a message has been signed
by a particular key. For example, a point-of-sale system might want a
customer to sign a transaction so it can be processed
asynchronously. You can do this in LIGO using the `key` and
`signature` types.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is
> because that would require storing a private key on chain, at which
> point it is not... private anymore.

<Syntax syntax="cameligo">

```cameligo group=c
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
const check_signature =
  (pk: key, signed: signature, msg: bytes) =>
  Crypto.check(pk, signed, msg);
```

</Syntax>


## Contract's Own Address

Often you want to get the address of the contract being executed. You
can do it with `Tezos.get_self_address`.

> ⚠️ Due to limitations in Michelson, `Tezos.get_self_address` in a
> contract is only allowed at the top-level. Using it in an embedded
> function will cause an error.

<Syntax syntax="cameligo">

```cameligo group=d
let current_addr : address = Tezos.get_self_address ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=d
const current_addr = Tezos.get_self_address();
```

</Syntax>

## Origination of a contract

`Tezos.create_contract` allows you to originate a contract given its code, delegate (if any), initial balance and initial storage.
The return value is a pair of type `(operation * address)`.

> ⚠️ Due to limitations in Michelson, `Tezos.create_contract` first argument
> must be inlined and must not contain references to free variables

<Syntax syntax="cameligo">

```cameligo group=e
let origination : operation * address = Tezos.create_contract
  (fun (p : nat) (s : string) -> ([], s))
  None
  3tz
  "initial_storage"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=e
const origination = Tezos.create_contract(
  (p: nat, s: string) => [list([]), s],
  None(),
  3tez,
  "initial_storage"
);
```

</Syntax>

<!-- updated use of entry -->