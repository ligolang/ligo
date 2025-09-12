---
id: comparing
title: Comparing
---

import Syntax from '@theme/Syntax';

Boolean values are the result of comparisons of values. Numbers and
strings are completely ordered. Booleans can be compared for
equality. Two values need to be of the same type to be compared, but
not all values of the same type can be compared: only those with <em>comparable types</em> (a concept directly lifted from Michelson)
such as `int`, `nat`, `string`, and `bool` itself. The comparison
operators are overloaded so they are defined on all comparable types.

<Syntax syntax="cameligo">

```cameligo group=comparing
let a : bool = 1 = 1   // equality (true)
let b : bool = 1 <> 0  // inequality (true)
let c : bool = 1 > 0   // greater than (true)
let d : bool = 0 < 1   // lower than (true)
let e : bool = 0 >= 0  // greater than or equal (true)
let f : bool = 0 <= 0  // lower than or equal (true)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=comparing
const a : bool = 1 == 1;  // equality (true)
const b : bool = 1 != 0;  // inequality (true)
const c : bool = 1 > 0;   // greater than (true)
const d : bool = 0 < 1;   // lower than (true)
const e : bool = 0 >= 0;  // greater than or equal (true)
const f : bool = 0 <= 0;  // lower than or equal (true)
```

</Syntax>
