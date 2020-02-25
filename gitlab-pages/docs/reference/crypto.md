---
id: crypto-reference
title: Crypto — Cryptographic functions
---

## Crypto.blake2b(data: bytes): bytes

Runs the [blake2b hash algorithm](https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2)
over the given `bytes` data and returns a `bytes` representing the hash.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo
function hasherman_blake (const s: bytes) : bytes is blake2b(s)
```

<!--CameLIGO-->

```cameligo
let hasherman_blake (s: bytes) : bytes = Crypto.blake2b s
```

<!--ReasonLIGO-->

```reasonligo
let hasherman_blake = (s: bytes) => Crypto.blake2b(s);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Crypto.sha256(data: bytes) : bytes

Runs the [sha256 hash algorithm](https://en.wikipedia.org/wiki/SHA-2) over the given
`bytes` data and returns a `bytes` representing the hash.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function hasherman (const s : bytes) : bytes is
  begin skip end with sha_256(s)
```

<!--CameLIGO-->
```cameligo
let hasherman (s : bytes) : bytes =
   Crypto.sha256 s
```

<!--ReasonLIGO-->
```reasonligo
let hasherman = (s: bytes): bytes => Crypto.sha256(s);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Crypto.sha512(data: bytes) : bytes

Runs the [sha512 hash algorithm](https://en.wikipedia.org/wiki/SHA-2) over the given
`bytes` data and returns a `bytes` representing the hash.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo
function hasherman512 (const s: bytes) : bytes is sha_512(s)
```

<!--CameLIGO-->

```cameligo
let hasherman512 (s: bytes) : bytes = Crypto.sha512 s
```

<!--ReasonLIGO-->

```reasonligo
let hasherman512 = (s: bytes) => Crypto.sha512(s);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Crypto.hash_key(k: key) : key_hash

Hashes a key for easy comparison and storage.

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

## Crypto.check(pk: key, signed: signature, data: bytes) : bool

Check that a message has been signed by a particular key.

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
