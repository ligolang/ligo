---
id: updating
title: Updating
---

import Syntax from '@theme/Syntax';

The function `List.update_with` enables the replacement of elements of
a given list according to a boolean function: if the call of that
function on a element is true, then the element is replaced, otherwise
it remains.

<Syntax syntax="cameligo">

```cameligo group=list_updating
let nats = [0; 1; 2; 3; 4]
// evens_zeroed = [0; 1; 0; 3; 0]
let evens_zeroed = List.update_with (fun x -> x mod 2 = 0n) 0 nats
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=list_updating
const nats = list([0, 1, 2, 3, 4]);
// evens_zeroed == list([0, 1, 0, 3, 0])
const evens_zeroed = List.update_with(x => x % 2 == 0n, 0, nats);
```

</Syntax>

The function `List.update` enables the selective replacement of
elements of a given list according to a function that returns an
optional value, instead of a boolean as `List.update_with` above.

<Syntax syntax="cameligo">

That function takes an element and returns an optional value: if that
value is `None`, then the element is left unchanged, otherwise, if the
value is `Some v`, then the element is replaced in the resulting list
by `v`.

```cameligo group=list_updating
let f x = if x mod 2 = 0n then None else Some (x*x)
// odds = [0; 1; 2; 9; 4]
let odds_squared = List.update f nats
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

That function takes an element and returns an optional value: if that
value is `None()`, then the element is left unchanged, otherwise, if
the value is `Some(v)`, then the element is replaced in the resulting
list by `v`.

```jsligo group=list_updating
const f = x => x % 2 == 0n ? None() : Some(x*x);
// odds == list([0, 1, 2, 9, 4])
const odds_squared = List.update(f, nats);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
