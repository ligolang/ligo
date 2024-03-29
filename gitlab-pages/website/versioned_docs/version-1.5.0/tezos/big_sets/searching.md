---
id: searching
title: Searching
---

import Syntax from '@theme/Syntax';

The predicate `Big_set.mem` tests for membership in a given big set.

<Syntax syntax="cameligo">

```cameligo group=big_set_membership
let my_big_set : int big_set = Big_set.literal [3; 2; 2; 1]
let contains_3 : bool = Big_set.mem 3 my_big_set // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_set_membership
const my_big_set: big_set<int> = Big_set.literal(list([3, 2, 2, 1]));
const contains_3: bool = Big_set.mem(3, my_big_set); // == true
```

</Syntax>
