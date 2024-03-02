---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

Sets are unordered collections of values of the same type, like lists
are ordered collections. Like the mathematical sets and lists, sets
can be empty and, if not, elements of sets in LIGO are *unique*,
whereas they can be repeated in a *list*.

Like lists, the type of sets is parameterised over the type of its
elements. Like list elements, set elements must all have the same
type.

The empty set is denoted by the predefined value `Set.empty`. A
non-empty set can be built by using the function `Set.literal` which
takes a list of elements and returns a set containing them, and only
them.

<Syntax syntax="cameligo">

```cameligo group=sets
let empty_set : int set = Set.empty
let my_set : int set = Set.literal [3; 2; 2; 1]
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
const empty_set: set<int> = Set.empty;
const my_set: set<int> = Set.literal(list([3, 2, 2, 1]));
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

> Note: The element `2` is repeated in the list, but not in the set
> made from it.

Set elements are internally sorted by increasing values, so the type
of the elements must be *comparable*, that is, they obey a total order
(any two elements can be compared).
