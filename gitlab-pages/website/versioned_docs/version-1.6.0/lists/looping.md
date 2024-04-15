---
id: looping
title: Looping
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">
There is no loop over lists in CameLIGO.

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

One can iterate through all the elements of a list, from left to
right, thanks to a loop of the form `for (const <variable> of <list>) <block>`. It means that the `<block>` of statements (or a single
statement) will be computed once for each `<variable>` ranging over
the elements of the list `<list>` from left to right.

Here is an example where the integers in a list are summed up, and the
sum is zero if the list is empty:

```jsligo group=list_looping
function sum_list (l: list<int>) {
  let sum = 0;
  for (const i of l) sum = sum + i;
  return sum;
};
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
