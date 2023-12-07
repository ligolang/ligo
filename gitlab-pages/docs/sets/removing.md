---
id: removing
title: Removing
---

import Syntax from '@theme/Syntax';

The function `Set.remove` creates a set containing the elements of a
given set, without a given element. If the element is not already
present, the new set is the same as the old one, as expected.

<Syntax syntax="cameligo">

```cameligo group=set_removing
let my_set : int set = Set.literal [3; 2; 2; 1]
let new_set = Set.remove 3 my_set
let contains_3 = Set.mem 3 new_set // = false
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_removing
const my_set: set<int> = Set.literal(list([3, 2, 2, 1]));
const new_set = Set.remove(3, my_set);
const contains_3 = Set.mem(3, new_set); // == false
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
