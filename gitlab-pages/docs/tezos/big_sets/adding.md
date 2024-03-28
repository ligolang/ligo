---
id: adding
title: Adding
---

import Syntax from '@theme/Syntax';

Adding an element to a big set is done by calling the function
`Big_set.add`. If the element was already present in the given big
set, the resulting big set is the same as the original one.

<Syntax syntax="cameligo">

```cameligo group=big_set_adding
let my_big_set : int big_set = Big_set.literal [3; 2; 2; 1]
let with_4 : int big_set = Big_set.add 4 my_big_set
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_set_adding
const my_big_set: big_set<int> = Big_set.literal(list([3, 2, 2, 1]));
const with_4: big_set<int> = Big_set.add(4, my_big_set);
```

</Syntax>
