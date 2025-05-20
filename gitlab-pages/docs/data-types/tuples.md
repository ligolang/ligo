---
title: Tuples
---

import Syntax from '@theme/Syntax';

Tuples are a list of a specific number of values in a specific order.
The elements in a tuple are called its _components_.

Unlike lists and sets, tuples have a specific size and cannot add or remove components.
As a result, you can access tuple components in random order by their index (or position).
Also unlike lists and sets, tuples can contain components of different types.

A common type of tuple is the _pair_, a tuple of two components.
For example, if you wanted to store coordinates on a two-dimensional grid, you could use a pair `(x, y)`.
These coordinates are in a specific order, so `(x, y)` is not equivalent to `(y, x)`.
The number of components is fixed, so you cannot add a third component to the tuple after it is defined and make it `(x, y, z)`.
In this way, the number of components is part of the type of the tuple, so a tuple with two components does not have the same type as a tuple with three components and therefore cannot be used in the same places, like function parameters.

:::note

One important use of tuples is in the return values of contract entrypoints.
Each entrypoint returns a tuple, of which the first component is a list of operations to run and the second is the new value of the contract storage.

Tuples also often appear as function parameters, which can be confusing if you assume that they receive individual values.
For example, the function that you use for folding (as in [Folding lists](./lists#folding-lists) and [Folding sets](./sets#folding-sets)) receives a pair as a parameter instead of two separate values.

:::

<Syntax syntax="cameligo">

:::note

Parentheses are optional, so the tuple `(x,y)` can be written as `x,y` if the context captures neither `x` nor `y`.

:::

Tuple components can have different types.
If a pair contains a first component of type `t_1` and a second component of type `t_2`, its type is written `t_1 * t_2`.
If there are more components, the type can be `t1 * t2 * ... * t_n`.
In this way, you can think of tuple types as products of types.

Tuple types do not have to be defined before they can be used, as in  this example, which implicitly creates a pair of strings:

```cameligo group=tuple
let friends = "Alice", "Bob" // Parentheses are optional
```

However, defining the type of the tuple explicitly can make code easier to read, as in this example:

```cameligo group=tuple
type couple = string * string
let friends : couple = "Alice", "Bob" // Parentheses are optional
```

</Syntax>

<Syntax syntax="jsligo">

Tuple components can have different types.
If a pair contains a first component of type `t_1` and a second component of type `t_2`, its type is written `[t_1, t_2]`.
If there are more components, the type can be `[t1, t2, ..., t_n]`.
In this way, you can think of tuple types as products of types.

Tuple types do not have to be defined before they can be used, as in this example, which implicitly creates a pair of strings:

```jsligo group=tuple
const friends = ["Alice", "Bob"];
```

However, defining the type of the tuple explicitly can make code easier to read, as in this example:

```jsligo group=tuple_alt
type couple = [string, string];
const friends: couple = ["Alice", "Bob"];
```

</Syntax>

You can nest tuples to create complex custom data types.
This example shows a tuple whose second component is a tuple:

<Syntax syntax="cameligo">

```cameligo group=nested
type nested_tuple = int * (string * nat)
let my_nested_value = (-5, ("Hello", 12))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=nested
type nested_tuple = [int, [string, nat]]
const my_nested_value = [-5, ["Hello", 12]];
```

</Syntax>

## Accessing components (destructuring)

To get the components of a tuple, you can destructure the tuple into separate variables.
This example assigns the values of a pair to two variables:

<Syntax syntax="cameligo">

```cameligo group=destructuring
let friends = "Alice", "Bob" // Parentheses are optional
let alice, bob = friends
```

That single definition creates two variables in the current scope: `alice` and `bob`.
If you are not going to use any of these variables, give them the name `_` or a name that starts with an underscore to make them into silent variables.
Using silent variables prevents the LIGO compiler from warning you about unused variables, as in this example:

```cameligo group=destructuring
let alice, _bob = friends (* This alice shadows the previous one *)
```

You can destructure nested tuples in the same manner:

```cameligo group=destructuring
let deep = (1, (2n, "Hello"))
let _, (_, greeting) = deep // greeting = "Hello"
```

If you don't want to create variables, you can access the components of tuples by their indexes.
This example gets the second component of the second component of the nested tuple in the previous example:

```cameligo group=destructuring
let film = deep.1.1 ^ ", Dolly!" // film = "Hello, Dolly!"
```

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=destructuring
const friends = ["Alice", "Bob"];
const [alice, bob] = friends;
```

That single definition creates two variables in the current scope: `alice` and `bob`.
If you are not going to use any of these variables, give them the name `_` or a name that starts with an underscore to make them into silent variables.
Using silent variables prevents the LIGO compiler from warning you about unused variables, as in this example:

```jsligo group=destructuring
const [alice2, _bob] = friends;
```

You can destructure nested tuples in the same manner:

```jsligo group=destructuring
const deep = [1, [2n, "Hello"]];
const [_x, [_y, greeting]] = deep; // greeting == "Hello"
```

If you don't want to create variables, you can access the components of tuples by their indexes.
This example gets the second component of the second component of the nested tuple in the previous example:

```jsligo group=destructuring
const film = deep[1][1] + ", Dolly!" // film == "Hello, Dolly!"
```

</Syntax>

:::note

Tuple indexes start at zero.

:::
