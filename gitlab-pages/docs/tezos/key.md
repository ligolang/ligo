---
id: key
title: Keys
---

import Syntax from '@theme/Syntax';

The `key` type in LIGO is used for Tezos public keys. Do not confuse
them with map keys. Keys are made by casting strings. Beware of
failure if the key is invalid.

Here is how you can define a key.

<Syntax syntax="cameligo">

```cameligo group=key
let my_key : key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=key
const my_key : key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
```

</Syntax>
