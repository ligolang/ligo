---
id: lambdas
title: Lambdas
---

import Syntax from '@theme/Syntax';

We have seen that function are declarated by given them a name,
parameters and a body. Sometimes we need a function that is used only
once, and hence does not deserve to have a name (perhaps not only
difficult to come up with, but also would incur a slight risk of
clutter of the scope, or even capture). In other words, we need
*function expressions*. Those are sometimes called *lambdas* or
*anonymous functions*.

First, let us see how to declare functions with function expressions,
instead of the dedicated syntax we already know. This is a useful
exercise, before we see more useful use-cases.

<Syntax syntax="cameligo">

Function expressions are introduced with the `fun` keyword, followed
by the parameters, then an arrow `->`, followed by the body (an
expression):

```cameligo group=lambdas
let sum = fun x y -> x + y      // Uncurried
let add = fun (x,y) -> x + y    // Curried
let increment = fun x -> x + 1
```

We now see that `fun x y -> x + y` and `fun (x,y) -> x + y` are
expressions, and we can use them *without a name* in contexts where
functions of type `int -> int -> int`, respectively `int * int ->
int`, are valid. In the dedicated sections on lists, maps and sets, we
present how lambdas are most useful.

</Syntax>


<Syntax syntax="jsligo">

Function expressions are called *arrow functions* in JsLIGO:

```jsligo group=lambdas
const sum = (x,y) => x + y;
const increment = x => x + 1; // Or (x) => x + 1
```

Note that when there is a single parameter that is not given a type,
the parentheses are not necessary, but they are if the return type is
give, like so:

```jsligo group=lambdas
const decrement = (x) : int => x - 1;
```

We understand that `(x,y) => x + y` is an expression, and we can use
it *without a name* in contexts where functions of type `(x: int, y: int) =>
int` are valid. In the dedicated sections on lists, maps and sets, we
present how lambdas are most useful.

> Note: When a function takes *one* argument that is a tuple,
> parentheses are mandatory, like so:

```jsligo group=lambdas
const comp_sum = ([x,y]) => x + y;
```

> That function is different from `(x,y) => x + y`, which takes *two*
> arguments. In other words, `sum` has type `(x: int, y: int) => int`
> and `comp_sum` has type `([x,y] : [int,int]) => int`.


</Syntax>
