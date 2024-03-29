---
id: accessing
title: Accessing
---

import Syntax from '@theme/Syntax';

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
