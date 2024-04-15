---
id: or
title: Or
---

import Syntax from '@theme/Syntax';


<Syntax syntax="cameligo">

The logical disjunction ("or") is implemented by the binary operator
`||`:


```cameligo group=disjunction
let or_1 : bool = false || true  // true
let or_2 : bool = false || false // false
let or_3 : bool = true  || true  // true
let or_4 : bool = true  || false // true
```

Note that you can also use the keyword `or` instead of the symbol `||`
(as in OCaml):

```cameligo group=disjunction
let or_1 : bool = false or true  // true
let or_2 : bool = false or false // false
let or_3 : bool = true  or true  // true
let or_4 : bool = true  or false // true
```

</Syntax>

<Syntax syntax="jsligo">

The logical disjunction ("or") is implemented by the binary operator
`||`.

```jsligo group=disjunction
const or_1 : bool = false || true;  // true
const or_2 : bool = false || false; // false
const or_3 : bool = true  || true;  // true
const or_4 : bool = true  || false; // true
```

</Syntax>
