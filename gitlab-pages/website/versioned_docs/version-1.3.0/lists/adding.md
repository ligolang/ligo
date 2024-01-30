---
id: adding
title: Adding
---

import Syntax from '@theme/Syntax';

Lists can be augmented by adding an element before the head (or, in
terms of stack, by *pushing an element on top*). This operation is
usually called *consing* in functional languages.

<Syntax syntax="cameligo">

The *cons operator* is infix and noted "`::`". It is not symmetric: on
the left lies the element to cons, and, on the right, a list on which
to cons.

```cameligo group=consing
let short_list = [1; 2; 2]
// long_list = [5; 1; 2; 2]
let long_list : int list = 5 :: short_list
```

There is also a predefined function `List.cons`:

```cameligo group=consing
// longer_list = [6; 5; 1; 2; 2]
let longer_list = List.cons 6 long_list
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

The *cons operator* is infix and noted "`, ...`". It is not symmetric:
on the left lies the element to cons, and, on the right, a list on
which to cons.

```jsligo group=consing
const short_list = list([1,2,2]);
// long_list == [5,1,2,2]
const long_list = list([5, ...short_list]);
```

There is also a predefined function `List.cons`:

```jsligo group=consing
// longer_list == list([6, 5, 1, 2, 2])
const longer_list = List.cons(6, long_list);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
