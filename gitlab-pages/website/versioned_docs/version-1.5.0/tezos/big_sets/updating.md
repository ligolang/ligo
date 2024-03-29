---
id: updating
title: Updating
---

import Syntax from '@theme/Syntax';

Previous sections show how to add and remove an element from a given
big set. The function `Big_set.update` can do both depending on a
boolean value: if true, then the given value will be added to the big
set, otherwise it will be removed (if present).

<Syntax syntax="cameligo">

```cameligo group=big_set_updating
let nats : int big_set = Big_set.literal [3; 2; 2; 1]
let big_set_with_5 = Big_set.update 5 true nats
let big_set_without_3 = Big_set.update 3 false nats
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_set_updating
const nats: big_set<int> = Big_set.literal(list([3, 2, 2, 1]));
const big_set_with_5 = Big_set.update(5, true, nats);
const big_set_without_3 = Big_set.update(3, false, nats);
```

</Syntax>
