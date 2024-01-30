---
id: iterating
title: Iterating
---

import Syntax from '@theme/Syntax';

An *iterated operation* is a fold over a set that returns the value of
type `unit`, that is, its only use is to produce side-effects. This
can be useful if, for example, you would like to check that each
element of a set is within a certain range, and fail with an error
otherwise.

The predefined functional iterator implementing the iterated operation
over sets is called `Set.iter`. In the following example, a set is
iterated to check that all its elements (integers) are greater than
`3`.

<Syntax syntax="cameligo">

```cameligo group=set_iterating
let assert_all_greater_than_3 (s : int set) : unit =
  Set.iter (fun i -> assert (i > 3)) s
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_iterating
const assert_all_greater_than_3 =
  (s: set<int>) : unit => Set.iter(i => assert(i > 3), s);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
