---
id: adding
title: Adding
---

import Syntax from '@theme/Syntax';

Adding a binding to a map is done by calling the function
`Map.add`. If the key was already present in the given map, the
corresponding value is updated.

<Syntax syntax="cameligo">

```cameligo group=map_adding
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let new_map = Map.add 3 "three" my_map
let contains_3 = Map.mem 3 new_map // = true
```

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_adding
const my_map: map<int,string> = Map.literal(list([[1,"one"],[2,"two"]]));
const new_map = Map.add(3, "three", my_map);
const contains_3 = Map.mem(3, new_map); // == true
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
