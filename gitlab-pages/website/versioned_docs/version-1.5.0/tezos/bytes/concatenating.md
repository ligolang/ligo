---
id: concatenating
title: Concatenating
---

import Syntax from '@theme/Syntax';

Two or more bytes can be concatenated.

<Syntax syntax="cameligo">

```cameligo group=concatenating
let two : bytes = Bytes.concat 0x70 0xAA
let three : bytes = Bytes.concats [0x70; 0xAA; 0xFF]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=concatenating
const two: bytes = Bytes.concat(0x70, 0xAA);
const three: bytes = Bytes.concats(list([0x70, 0xAA, 0xFF]));
```

</Syntax>
