---
id: sizing
title: Sizing
---

import Syntax from '@theme/Syntax';

The predefined functions `Set.size` and `Set.cardinal` return the
number of elements in a given set.

<Syntax syntax="cameligo">

```cameligo group=cardinal
let my_set : int set = Set.literal [3; 2; 2; 1]
let cardinal : nat = Set.size my_set // = 3
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=cardinal
const my_set: set<int> = Set.literal(list([3, 2, 2, 1]));
const cardinal : nat = Set.size(my_set); // == 3
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
