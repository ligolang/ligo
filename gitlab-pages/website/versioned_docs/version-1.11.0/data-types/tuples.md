---
title: Tuples
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

## Accessing

If we want to get the first and second names of a pair, we can use
destructuring. Destructuring a tuple allows us to give names to the
elements inside the tuple:

<Syntax syntax="cameligo">

```cameligo group=destructuring
let friends = "Alice", "Bob" // Parentheses are optional
let alice, bob = friends
```

That single definition actually introduces in the current scope *two*
constants, `alice` and `bob`. Sometimes we might want to ignore a
component of the tuple, in which case we use the character `_`:

```cameligo group=destructuring
let alice, _ = friends
```

Alternatively, if we still want to give a meaningful name to a useless
component, we can use a silent variable for it, by prefixing it with
`_`:

```cameligo group=destructuring
let alice, _bob = friends // This alice shadows the previous one
```

We can destructure nested tuples in the same manner:

```cameligo group=destructuring
let deep = (1, (2n, "Hello"))
let _, (_, greeting) = deep // greeting = "Hello"
```

This works well if we want to give a name to a component (like
`greeting` above), but we might simply want the value of a component
without naming it. In that case, we use the binary operator `.`:

```cameligo group=destructuring
let film = deep.1.1 ^ ", Dolly!" // film = "Hello, Dolly!"
```

The first component has index `0`, the second `1` etc.

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=destructuring
const friends = ["Alice", "Bob"];
const [alice, bob] = friends;
```

That single definition actually introduces in the current scope *two*
constants, `alice` and `bob`. Alternatively, if we still want to give
a meaningful name to a useless component, we can use a silent variable
for it, by prefixing it with `_`:

```jsligo group=destructuring
const [alice2, _bob] = friends;
```

Note how we renamed `alice` as `alice2` in order to avoid a collision
(redefinition) with previous one in the same top-level scope.

We can destructure nested tuples in the same manner:

```jsligo group=destructuring
const deep = [1, [2n, "Hello"]];
const [_x, [_y, greeting]] = deep; // greeting == "Hello"
```

This works well if we want to give a name to a component (like
`greeting` above), but we might simply want the value of a component
without naming it. In that case, we use the binary operator `[]`:

```jsligo group=destructuring
const film = deep[1][1] + ", Dolly!" // film == "Hello, Dolly!"
```

The first component has index `0`, the second `1` etc.

</Syntax>
