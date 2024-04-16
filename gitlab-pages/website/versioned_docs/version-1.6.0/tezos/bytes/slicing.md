---
id: slicing
title: Slicing
---

import Syntax from '@theme/Syntax';

Bytes can be extracted using the predefined function `Bytes.sub`. The
first parameter is the start index and the second is the number of
bytes of the slice we want. Keep in mind that the first byte in a
sequence has index `0n`.

<Syntax syntax="cameligo">

```cameligo group=slicing
let large = 0x12345678
let slice = Bytes.sub 1n 2n large // sub = 0x3456
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=slicing
const large = 0x12345678;
const slice = Bytes.sub(1n, 2n, large); // sub == 0x3456
```

</Syntax>
