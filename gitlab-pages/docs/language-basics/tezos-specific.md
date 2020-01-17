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

> ⚠️ There should be a message here about the dangers of using `UNPACK` on untrusted
data, perhaps about executable code?

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
let check_hash_key = (kh1_k2: (key_hash, key)) : (bool, key_hash) => {
  let kh1, k2 = kh1_k2;
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

> ⚠️ There is no way to *generate* a signed message in LIGO. This is because that
would require storing a private key on chain, at which point it isn't very private
anymore.

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
let check_signature = (param: (key, signature, bytes)) : bool => {
  let pk, signed, msg = param;
  Crypto.check(pk, signed, msg);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->
 
