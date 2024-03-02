---
id: mapping
title: Mapping
---

import Syntax from '@theme/Syntax';

We may want to change all the values of a given map by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over maps is called `Map.map`. It
takes a binding, that is, a key and its associated value in the map,
and computes a new value for that key.

In the following example, from a map from integers to integers is made
a map whose values are the sum of the keys and values of each binding.

<Syntax syntax="cameligo">

```cameligo group=map_mapping
let my_map : (int, int) map = Map.literal [(0,0); (1,1); (2,2)]
// plus_one = Map.literal [(0,0); (1,2); (2,4)]
let plus_one = Map.map (fun (k,v) -> k + v) my_map
```

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_mapping
const my_map: map<int,int> = Map.literal(list([[0,0], [1,1], [2,2]]));
// plus_one == Map.literal(list([[0,0],[1,2],[2,4]]))
const plus_one = Map.map(([k,v]) => k + v, my_map);
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
