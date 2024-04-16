---
id: hash_key
title: Hash of Keys
---

import Syntax from '@theme/Syntax';

It is often desirable to hash a public key. In Michelson, certain data
structures, such as maps, will not allow the use of the `key`
type. Even if this were not the case, hashes are much smaller than
keys, and storage on blockchains comes at a premium cost. You can hash
keys with the predefined function `Crypto.hash_key` returning a value
of type `key_hash`.

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
