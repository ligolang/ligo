---
id: adding
title: Adding
---

import Syntax from '@theme/Syntax';

Adding an element to a set is done by calling the function
`Set.add`. If the element was already present in the given set, the
resulting set is the same as the original one.

<Syntax syntax="cameligo">

```cameligo group=set_adding
let my_set : int set = Set.literal [3; 2; 2; 1]
let with_4 : int set = Set.add 4 my_set
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_adding
const my_set: set<int> = Set.literal(list([3, 2, 2, 1]));
const with_4: set<int> = Set.add(4, my_set);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
