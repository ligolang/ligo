---
id: mapping
title: Mapping
---

import Syntax from '@theme/Syntax';

We may want to change all the elements of a given list by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over lists is called `List.map` and
is used as follows.

<Syntax syntax="cameligo">

```cameligo group=map_lists
let plus_one = List.map (fun i -> i + 1) [6; 2; 3; 3]
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_lists
const plus_one = List.map (i => i + 1, list([6,2,3,3]));
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
