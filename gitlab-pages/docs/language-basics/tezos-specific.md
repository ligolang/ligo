---
id: tezos-specific
title: Tezos Domain-Specific Operations
---

LIGO is a programming language for writing Tezos smart contracts. It
would be a little odd if it did not have any Tezos specific
functions. This page will tell you about them.

## Pack and Unpack

Michelson provides the `PACK` and `UNPACK` instructions for data
serialization.  The former converts Michelson data structures into a
binary format, and the latter reverses that transformation. This
functionality can be accessed from within LIGO.

> ⚠️ `PACK` and `UNPACK` are Michelson instructions that are intended
> to be used by people that really know what they are doing. There are
> several risks and failure cases, such as unpacking a lambda from an
> untrusted source or casting the result to the wrong type. Do not use
> the corresponding LIGO functions without doing your homework first.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=a
function id_string (const p : string) : option (string) is block {
  const packed : bytes = bytes_pack (p)
} with (Bytes.unpack (packed) : option (string))
```

> Note that `bytes_unpack` is *deprecated*.

<!--CameLIGO-->
```cameligo group=a
let id_string (p : string) : string option =
  let packed: bytes = Bytes.pack p in
  (Bytes.unpack packed : string option)
```

<!--ReasonLIGO-->
```reasonligo group=a
let id_string = (p : string) : option (string) => {
  let packed : bytes = Bytes.pack (p);
  (Bytes.unpack(packed) : option (string));
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Hashing Keys

It is often desirable to hash a public key. In Michelson, certain data
structures such as maps will not allow the use of the `key` type. Even
if this were not the case, hashes are much smaller than keys, and
storage on blockchains comes at a cost premium. You can hash keys with
a predefined functions returning a value of type `key_hash`.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=b
function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is
  block {
    var ret : bool := False;
    var kh2 : key_hash := crypto_hash_key (k2);
    if kh1 = kh2 then ret := True else skip
  } with (ret, kh2)
```

<!--CameLIGO-->
```cameligo group=b
let check_hash_key (kh1, k2 : key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in
  if kh1 = kh2 then true, kh2 else false, kh2
```

<!--ReasonLIGO-->
```reasonligo group=b
let check_hash_key = ((kh1, k2) : (key_hash, key)) : (bool, key_hash) => {
  let kh2 : key_hash = Crypto.hash_key (k2);
  if (kh1 == kh2) { (true, kh2); } else { (false, kh2); }
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Checking Signatures

Sometimes a contract will want to check that a message has been signed
by a particular key. For example, a point-of-sale system might want a
customer to sign a transaction so it can be processed
asynchronously. You can do this in LIGO using the `key` and
`signature` types.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is
> because that would require storing a private key on chain, at which
> point it is not... private anymore.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=c
function check_signature
    (const pk     : key;
     const signed : signature;
     const msg    : bytes) : bool
  is Crypto.check (pk, signed, msg)
```

> Note that `crypto_check` is *deprecated*.

<!--CameLIGO-->
```cameligo group=c
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

<!--ReasonLIGO-->
```reasonligo group=c
let check_signature =
  ((pk, signed, msg) : (key, signature, bytes)) : bool =>
  Crypto.check (pk, signed, msg);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Contract's Own Address

Often you want to get the address of the contract being executed. You
can do it with `Tezos.self_address`.

> Note that `self_address` is *deprecated*.

> ⚠️ Due to limitations in Michelson, `Tezos.self_address` in a
> contract is only allowed at the top-level. Using it in an embedded
> function will cause an error.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=d
const current_addr : address = Tezos.self_address
```

<!--CameLIGO-->
```cameligo group=d
let current_addr : address = Tezos.self_address
```

<!--ReasonLIGO-->
```reasonligo group=d
let current_addr : address = Tezos.self_address;
```

<!--END_DOCUSAURUS_CODE_TABS-->
