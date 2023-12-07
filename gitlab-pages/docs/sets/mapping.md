---
id: mapping
title: Mapping
---

import Syntax from '@theme/Syntax';

We may want to change all the elements of a given set by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over sets is called `Set.map` and is
used as follows.

<Syntax syntax="cameligo">

```cameligo group=set_mapping
let s : int set = Set.literal [5; 1; 2; 2]
// plus_one = Set.literal [6; 2; 3]
let plus_one : int set = Set.map (fun i -> i + 1) s
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_mapping
const s: set<int> = Set.literal(list([5,1,2,2]));
// plus_one == Set.literal(list([6,2,3]))
const plus_one: set<int> = Set.map(i => i + 1, s);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
