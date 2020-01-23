---
id: tezos-specific
title: Tezos Domain-Specific Operations
---

LIGO is a language for writing Tezos smart contracts. It would be a little odd if
it didn't have any Tezos specific functions. This page will tell you about them.

## Pack and Unpack

Michelson provides the `PACK` and `UNPACK` instructions for data serialization.
`PACK` converts Michelson data structures to a binary format, and `UNPACK`
reverses it. This functionality can be accessed from within LIGO.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(string))
```

<!--CameLIGO-->
```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

<!--ReasonLIGO-->
```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Hashing Keys

It's often desirable to hash a public key. In Michelson, certain data structures
such as maps will not allow the use of the `key` type. Even if this weren't the case
hashes are much smaller than keys, and storage on blockchains comes at a cost premium.
You can hash keys with the `key_hash` type and associated built in function.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function check_hash_key (const kh1 : key_hash; const k2 : key) : bool * key_hash is block {
  var ret : bool := False ;
  var kh2 : key_hash := crypto_hash_key(k2) ;
  if kh1 = kh2 then ret := True else skip; 
} with (ret, kh2)
```

<!--CameLIGO-->
```cameligo
let check_hash_key (kh1, k2: key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in
  if kh1 = kh2
  then (true, kh2)
  else (false, kh2)
```

<!--ReasonLIGO-->
```reasonligo
let check_hash_key = ((kh1, k2): (key_hash, key)) : (bool, key_hash) => {
  let kh2 : key_hash = Crypto.hash_key(k2);
  if (kh1 == kh2) {
    (true, kh2);
  }
  else {
    (false, kh2);
  }
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Checking Signatures

Sometimes a contract will want to check that a message has been signed by a
particular key. For example, a point-of-sale system might want a customer to
sign a transaction so it can be processed asynchronously. You can do this in LIGO
using the `key` and `signature` types.

> ⚠️ There is no way to *generate* a signed message in LIGO. This is because that would require storing a private key on chain, at which point it isn't very private anymore.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function check_signature
    (const pk: key;
     const signed: signature;
     const msg: bytes) : bool
  is crypto_check(pk, signed, msg)
```

<!--CameLIGO-->
```cameligo
let check_signature (pk, signed, msg: key * signature * bytes) : bool =
  Crypto.check pk signed msg
```

<!--ReasonLIGO-->
```reasonligo
let check_signature = ((pk, signed, msg): (key, signature, bytes)) : bool => {
  Crypto.check(pk, signed, msg);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->
 
## Getting The Contract's Own Address

Often you want to get the address of the contract being executed. You can do it with
`self_address`.

> ⚠️ Due to limitations in Michelson, self_address in a contract is only allowed at the entry-point level. Using it in a utility function will cause an error.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
const current_addr : address = self_address;
```

<!--CameLIGO-->
```cameligo
let current_addr : address = Current.self_address
```

<!--ReasonLIGO-->
```reasonligo
let current_addr : address = Current.self_address;
```

<!--END_DOCUSAURUS_CODE_TABS-->
