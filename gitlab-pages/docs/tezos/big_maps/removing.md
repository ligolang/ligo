---
id: removing
title: Removing
---

import Syntax from '@theme/Syntax';

The function `Big_map.remove` creates a big map containing the
elements of a given big map, without a given element. If the element
is not already present, the new big map is the same as the old one, as
expected.

<Syntax syntax="cameligo">

```cameligo group=big_map_removing
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let new_map = Big_map.remove 2 my_map
let contains_3 = Big_map.mem 2 new_map // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_removing
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const new_map = Big_map.remove(2, my_map);
const contains_3 = Big_map.mem(2, new_map); // == false
```

</Syntax>
