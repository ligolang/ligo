---
id: looping
title: Looping
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">
There is no loop over lists in CameLIGO.

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

One can iterate through all the elements of a set, in increasing
order, thanks to a loop of the form `for (const <variable> of <set>) <block>`. It means that the `<block>` of statements (or a single
statement) will be computed once for each `<variable>` ranging over the
elements of the set `<set>` in increasing order.

Here is an example where the integers in a set are summed up.

```jsligo group=set_looping
function sum_elt (s: set<int>) {
  let sum = 0;
  for (const e of s) sum = sum + e;
  return sum;
};
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
