---
id: searching
title: Searching
---

import Syntax from '@theme/Syntax';

The predicate `Set.mem` tests for membership in a given set.

<Syntax syntax="cameligo">

```cameligo group=set_membership
let my_set : int set = Set.literal [3; 2; 2; 1]
let contains_3 : bool = Set.mem 3 my_set // = true
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_membership
const my_set: set<int> = Set.literal(list([3, 2, 2, 1]));
const contains_3: bool = Set.mem(3, my_set); // == true
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
