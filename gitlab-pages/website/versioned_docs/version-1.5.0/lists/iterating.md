---
id: iterating
title: Iterating
---

import Syntax from '@theme/Syntax';

The *iterated operation* is an iteration over the list with a unit
return value. It is useful to enforce certain invariants on the
element of a list, or else fail.

For example you might want to check that each value inside of a list
is within a certain range, and fail otherwise. The predefined
functional iterator implementing the iterated operation over lists is
called `List.iter`.

In the following example, a list is iterated to check that all its
elements (integers) are strictly greater than `3`.

<Syntax syntax="cameligo">

```cameligo group=iterating_lists
let assert_all_greater_than_three (l : int list) : unit =
  List.iter (fun i -> assert (i > 3)) l
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=iterating_lists
const assert_all_greater_than_three = (l: list<int>): unit =>
  List.iter (i => assert (i > 3), l);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
