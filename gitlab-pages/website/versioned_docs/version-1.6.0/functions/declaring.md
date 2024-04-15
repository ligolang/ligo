---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

LIGO functions are the basic building block of contracts.

The semantics of function calls in LIGO is that of a *copy of the
arguments but also of the environment*. This means that any mutation
(assignment) on variables outside the scope of the function will be
lost when the function returns, just as the mutations inside the
functions will be.

<Syntax syntax="cameligo">

Function declarations are introduced by the `let` keyword, like other
values. A difference with non-functional values is the succession of
parameters provided after the value name. This follows OCaml
syntax. For example:

```cameligo group=fun_decl
let add x y = x + y
let int_add (x: int) (y: int) : int = x + y
```

This means that `sum` is a function that takes an argument (value)
bound to parameter `x`, and returns a function that takes an argument
bound to parameter `y`, then returns `x+y`. This is called
[currying](https://en.wikipedia.org/wiki/Currying). This default
semantics enables
[partial application](https://en.wikipedia.org/wiki/Partial_application).

```cameligo group=curry
let add (x,y) = x + y         // Uncurried
let add_curry x y = add (x,y) // Curried
let increment = add_curry 1   // Partial application
let one = increment 0
```

The type of `increment` is `int -> int`, meaning that it is a function
taking an argument of type `int` and returning a value of type
`int`. Similarly, the type of `add_curry` is `int -> int -> int`,
which means `int -> (int -> int)` and explains how we can partially
apply it (only on its first argument) with the declaration of
`increment`.

By default, LIGO will warn about unused arguments inside
functions. In case we do not use an argument, we can use the wildcard
`_` to prevent warnings. Either use `_` instead of the argument
identifier:

```cameligo
let drop x _ = x
```

or use an identifier starting with wildcard:

```cameligo
let drop x _y = x  // _y silently ignored
```

Functions can capture variables in their bodies that are defined
outside, as shown above with the function `add_curry`. We could have
the same phenomenon with nested functions instead of two functions at
the same scope level:

```cameligo
let convoluted_doubling x =
  let add_x y = x + y  // x is bound by convoluted_doubling
  in add_x x
```

</Syntax>

<Syntax syntax="jsligo">

Function declarations can be introduced in two main ways: either by
the keyword `function` or the keyword `const` (the keyword `let` is
possible too, but defaulted to `const` in this instance). The latter
manner is preferred when the function body is an expression. For
example, here is how you define a basic function that sums two
integers:

```jsligo group=sum
const add = (x,y) => x + y;  // Single-expression body
const int_add = (x: int, y: int) : int => x + y
```

This means that `add` is a function that takes a pair of integers and
returns their sum.

If the body contains *statements* instead of a single expression, you
would use a block and a `return` statement:

```jsligo group=sum
const double_sum = (x: int, y: int) : int => {
  const doubleX = 2*x;
  const doubleY = 2*y;
  return doubleX + doubleY;
};
```

although it is arguably more readable to use `function`, like so:

```jsligo group=sum
function double_sum_bis (x: int, y: int) {
  const doubleX = 2*x;
  const doubleY = 2*y;
  return doubleX + doubleY;
};
```

Note that JsLIGO, like JavaScript, requires the `return` keyword to
indicate what is being returned. If `return` is not used, it will be
the same as `return []` (unit).

By default, LIGO will warn about unused arguments inside functions. In
case we do not use an argument, its name should start with `_` to
prevent warnings.

```jsligo
const drop = (x, _y) => x; // _y silently ignored
```

Functions can capture variables in their bodies that are defined
outside:

```jsligo group=sum
const double_incr = x => add (x,x) + 1; // add is captured from outside
```

We could have the same phenomenon with nested functions instead of two
functions at the same scope level:

```jsligo
function convoluted_doubling (x) {
  const addX = y => x + y;  // x is bound by convoluted_doubling
  return addX(x);
}
```

</Syntax>
