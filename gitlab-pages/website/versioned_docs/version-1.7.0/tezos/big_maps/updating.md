---
id: updating
title: Updating
---

import Syntax from '@theme/Syntax';

Previous sections show how to add and remove a binding from a given
big map. The function `Big_map.update` can do both depending whether
some value is given for the new binding or not: in the former case, a
new binding is added (and replaces any previous binding with the same
key); in the latter case, any binding with the same key is removed and
a new big map is returned.

<Syntax syntax="cameligo">

```cameligo group=big_map_updating
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let map_with_3 = Big_map.update 3 (Some "three") my_map
let contains_3 = Big_map.mem 3 map_with_3 // = true
let map_without_2 = Big_map.update 2 None my_map
let contains_2 = Big_map.mem 2 map_without_2 // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_updating
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const map_with_3 = Big_map.update (3, Some("three"), my_map);
const contains_3 = Big_map.mem(3, map_with_3); // == true
const map_without_2 = Big_map.update(2, None(), my_map);
const contains_2 = Big_map.mem (2, map_without_2); // == false
```

</Syntax>

When we want to update a big map, but also obtain the value of the
updated binding, we can use `Big_map.get_and_update`.

<Syntax syntax="cameligo">

```cameligo group=big_map_updating
// three = Some "three"
let three, map_without_3 = Big_map.get_and_update 3 None map_with_3
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_updating
// three == Some("three")
const [three, map_without_3] =
  Big_map.get_and_update(3, None(), map_with_3);
```

</Syntax>
