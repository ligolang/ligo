---
id: iterating
title: Iterating
---

import Syntax from '@theme/Syntax';

An *iterated operation* is a fold over the map that returns the value
of type `unit`, that is, its only use is to produce side-effects. This
can be useful if, for example, you would like to check that each value
of a map is within a certain range, and fail with an error otherwise.

The predefined functional iterator implementing the iterated operation
over maps is called `Map.iter`. It
takes a binding, that is, a key and its associated value in the map,
performs some side-effect and returns the unit value.

In the following example, a map is iterated to check that all its
integer values are greater than `3`.

<Syntax syntax="cameligo">

```cameligo group=map_iterating
let assert_all_greater_than_3 (m : (int, int) map) : unit =
  Map.iter (fun (_,v) -> assert (v > 3)) m  // The key is discarded
```

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_iterating
const assert_all_greater_than_3 =
  (m: map<int,int>) : unit => Map.iter(([_k,v]) => assert(v > 3), m);
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
