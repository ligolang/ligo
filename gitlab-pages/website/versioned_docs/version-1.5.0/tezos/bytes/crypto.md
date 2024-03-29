---
id: crypto
title: Cryptography
---

import Syntax from '@theme/Syntax';

One common use of bytes, beyond packing and unpacking, is
cryptography. The predefined module `Crypto` provides the following
hashing functions, which are efficient because they are natively
supported by the Michelson virtual machine:

<Syntax syntax="cameligo">

```cameligo skip
val blake2b : bytes -> bytes
val sha256 : bytes -> bytes
val sha512 : bytes -> bytes
val sha3 : bytes -> bytes
val keccak : bytes -> bytes
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const blake2b: bytes => bytes;
const sha256: bytes => bytes;
const sha512: bytes => bytes;
const sha3: bytes => bytes;
const keccak: bytes => bytes;
```

</Syntax>
