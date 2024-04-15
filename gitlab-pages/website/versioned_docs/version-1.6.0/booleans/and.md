---
id: and
title: And
---

import Syntax from '@theme/Syntax';

The logical conjunction ("and") is implemented by the binary operator
`&&`.

<Syntax syntax="cameligo">

```cameligo group=disjunction
let and_1 : bool = false && true  // false
let and_2 : bool = false && false // false
let and_3 : bool = true  && true  // true
let and_4 : bool = true  && false // false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=disjunction
const and_1 : bool = false && true;  // false
const and_2 : bool = false && false; // false
const and_3 : bool = true  && true;  // true
const and_4 : bool = true  && false; // false
```

</Syntax>
