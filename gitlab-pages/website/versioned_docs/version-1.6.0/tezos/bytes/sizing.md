---
id: sizing
title: Sizing
---

import Syntax from '@theme/Syntax';

In order to obtain the length of a sequence of bytes, use the
predefined function `Bytes.length` like so:

<Syntax syntax="cameligo">

```cameligo group=sizing
let len : nat = Bytes.length 0x0AFF // len = 2n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sizing
const len: nat = Bytes.length(0x0AFF); // len == 2n
```

</Syntax>
