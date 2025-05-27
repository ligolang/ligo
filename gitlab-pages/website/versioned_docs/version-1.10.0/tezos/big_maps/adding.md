---
id: adding
title: Adding
---

import Syntax from '@theme/Syntax';

Adding a binding to a big map is done by calling the function
`Big_Map.add`. If the key was already present in the given big map,
the corresponding value is updated.

<Syntax syntax="cameligo">

```cameligo group=big_map_adding
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let new_map = Big_map.add 3 "three" my_map
let contains_3 = Big_map.mem 3 new_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_adding
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const new_map = Big_map.add(3, "three", my_map);
const contains_3 = Big_map.mem(3, new_map); // == true
```

</Syntax>
