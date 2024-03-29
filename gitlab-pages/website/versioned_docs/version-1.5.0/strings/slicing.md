---
id: slicing
title: Slicing
---

import Syntax from '@theme/Syntax';

Substrings can be extracted using the predefined function
`String.sub`. The first character has index 0 and the interval of
indices for the substring has inclusive bounds.

<Syntax syntax="cameligo">

```cameligo group=slicing
let name  = "Alice"
let slice = String.sub 0n 1n name  // slice = "A"
```

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

The offset and length of the slice are natural number:

```jsligo group=slicing
const name = "Alice";
const slice = String.sub (0n, 1n, name); // slice == "A"
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>
