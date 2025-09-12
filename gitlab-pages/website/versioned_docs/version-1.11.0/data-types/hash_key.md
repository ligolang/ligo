---
id: hash_key
title: Hash of keys
---

import Syntax from '@theme/Syntax';

In some contexts, the hash of a public key is easier to use than the key itself.
For example, in Michelson, some data structures such as maps do not allow the `key` type.
Also, hashes are much smaller than keys.

You can hash keys with the predefined function `Crypto.hash_key`, which returns a value
of the type `key_hash`.

<Syntax syntax="cameligo">

```cameligo group=hash_key
let check_hash_key (kh1, k2 : key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2
  in (kh1 = kh2), kh2
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=hash_key
const check_hash_key = (kh1: key_hash, k2: key) => {
  let kh2 = Crypto.hash_key(k2);
  return [kh1 == kh2, kh2];
};
```

</Syntax>
