---
id: sizing
title: Sizing
---

import Syntax from '@theme/Syntax';

The predefined function `Map.size` returns the number of bindings
(entries) in a given map.

<Syntax syntax="cameligo">

```cameligo group=map_size
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let size : nat = Map.size my_map // = 2
```
Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_size
const my_map: map<int,string> = Map.literal(list([[1,"one"],[2,"two"]]));
const size: nat = Map.size(my_map); // == 2
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
