---
id: looping
title: Looping
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">
There is no loop over maps in CameLIGO.

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

One can iterate through all the bindings of a map, in increasing order
of the keys, thanks to a loop of the form `for (const <variable> of <map>) <block>`. It means that the `<block>` of statements (or a
single statement) will be computed once for each `<variable>` ranging
over the bindings (as pairs of keys and values) of the map `<map>` in
increasing order.

Here is an example where the values in a map are summed up.

```jsligo group=map_looping
function sum_val (m: map<int,int>) {
  let sum = 0;
  // The key is discarded.
  for (const [_key, val] of m) sum = sum + val;
  return sum;
};
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
