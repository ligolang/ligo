---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Tuples gather a given number of values in a specific order and those
values, called *components*, can be retrieved by their index
(position).  Probably the most common tuple is the *pair*. For
example, if we were storing coordinates on a two dimensional grid we
might use a pair `(x,y)` to store the coordinates `x` and `y`. There
is a *specific order*, so `(y,x)` is not equal to `(x,y)` in
general. The number of components is part of the type of a tuple, so,
for example, we cannot add an extra component to a pair and obtain a
triple of the same type: `(x,y)` has always a different type from
`(x,y,z)`, whereas `(y,x)` might have the same type as `(x,y)`.

> Parentheses are optional, so `(x,y)` can be written `x,y` if the
> context captures neither `x` nor `y`.

Tuple components can be of arbitrary types. A pair is a 2-tuple. If it
contains a first component of type `t_1` and a second component of
type `t_2`, its type is written `t_1 * t_2`. If more components: `t1 *
t2 * ... * t_n`. (We can think of tuple types as products of types.)
Tuple types do not have to be defined before they can be used:

```cameligo group=tuple
let friends = "Alice", "Bob" // Parentheses are optional
```

but it is sometimes more informative to define a type. Type
definitions are introduced with the keyword `type`, like value
definitions are with `let`. Instead of a value expression as a
right-hand side, we have a type expression:

```cameligo group=tuple
type couple = string * string
let friends : couple = "Alice", "Bob" // Parentheses are optional
```

</Syntax>

<Syntax syntax="jsligo">

Tuples gather a given number of values in a specific order and those
values, called *components*, can be retrieved by their index
(position).  Probably the most common tuple is the *pair*. For
example, if we were storing coordinates on a two dimensional grid we
might use a pair `[x, y]` to store the coordinates `x` and `y`. There
is a *specific order*, so `[y, x]` is not equal to `[x, y]` in
general. The number of components is part of the type of a tuple, so,
for example, we cannot add an extra component to a pair and obtain a
triple of the same type: `[x, y]` has always a different type from
`[x, y, z]`, whereas `[y, x]` might have the same type as `[x, y]`.

Tuple components can be of arbitrary types. A pair is a 2-tuple. If it
contains a first component of type `t_1` and a second component of
type `t_2`, its type is written `[t_1, t_2]`. If more components:
`[t1, t2, ..., t_n]`. (We can think of tuple types as products of
types.) Tuple types do not have to be defined before they can be used:

```jsligo group=tuple
const friends = ["Alice", "Bob"];
```

but it is sometimes more informative to define a type. Type
definitions are introduced with the keyword `type`, like value
definitions are with `const`. Instead of a value expression as a
right-hand side, we have a type expression:

```jsligo group=tuple_alt
type couple = [string, string];
const friends : couple = ["Alice", "Bob"];
```

</Syntax>
