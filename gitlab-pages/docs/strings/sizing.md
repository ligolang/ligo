---
id: sizing
title: Sizing
---

import Syntax from '@theme/Syntax';

The length of a string can be obtain by calling the predefined
functions `String.length` or `String.size`:

<Syntax syntax="cameligo">

```cameligo group=length
let length : nat = String.size "Alice" // length = 5n
```

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=length
const length : nat = String.size("Alice"); // length == 5n
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>
