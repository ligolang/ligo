---
id: removing
title: Removing
---

import Syntax from '@theme/Syntax';

The function `Big_set.remove` creates a big set containing the
elements of a given big set, without a given element. If the element
is not already present, the new big set is the same as the old one, as
expected.

<Syntax syntax="cameligo">

```cameligo group=big_set_removing
let my_big_set : int big_set = Big_set.literal [3; 2; 2; 1]
let new_big_set = Big_set.remove 3 my_big_set
let contains_3 = Big_set.mem 3 new_big_set // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_removing
const my_big_set: big_set<int> = Big_set.literal(list([3, 2, 2, 1]));
const new_big_set = Big_set.remove(3, my_big_set);
const contains_3 = Big_set.mem(3, new_big_set); // == false
```

</Syntax>
