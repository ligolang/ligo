---
id: removing
title: Removing
---

import Syntax from '@theme/Syntax';

The function `Map.remove` creates a map containing the elements of a
given map, without a given element. If the element is not already
present, the new map is the same as the old one, as expected.

<Syntax syntax="cameligo">

```cameligo group=map_removing
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let new_map = Map.remove 2 my_map
let contains_3 = Map.mem 2 new_map // = false
```

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_removing
const my_map: map<int,string> = Map.literal(list([[1,"one"],[2,"two"]]));
const new_map = Map.remove(2, my_map);
const contains_3 = Map.mem(2, new_map); // == false
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
