---
id: updating
title: Updating
---

import Syntax from '@theme/Syntax';

Previous sections show how to add and remove an element from a given
set. The function `Set.update` can do both depending on a boolean
value: if true, then the given value will be added to the set,
otherwise it will be removed (if present).

<Syntax syntax="cameligo">

```cameligo group=set_updating
let nats : int set = Set.literal [3; 2; 2; 1]
let set_with_5 = Set.update 5 true nats
let set_without_3 = Set.update 3 false nats
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_updating
const nats: set<int> = Set.literal(list([3, 2, 2, 1]));
const set_with_5 = Set.update(5, true, nats);
const set_without_3 = Set.update(3, false, nats);
```

</Syntax>

The function `Set.update` implements a one-value update. Sometime we
would like to provide a function that is applied in turn to *all* the
elements of the set, and specifies whether the element at hand has to
be discarded or replaced by a computed value. This is what
`Set.filter_map` does.

As an example, let us consider a function that removes all the even
numbers from a set.

<Syntax syntax="cameligo">

```cameligo group=set_updating
let f x = if x mod 2 = 0n then None else Some x
// odds = Set.literal [3, 1]
let odds = Set.filter_map f nats
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_updating
const f = x => x % 2 == 0n ? None() : Some(x);
// odds == Set.literal (list([3, 1]))
const odds = Set.filter_map(f, nats);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
