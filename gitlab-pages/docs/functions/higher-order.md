---
id: higher-order
title: Higher-order
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Functions can take a function as a parameter, or return a function:
this is known as *higher-order functions*. Perhaps the most obvious
example is to define a function that takes two functions and compose
them, like in mathematics.

```cameligo group=lambdas
let compose f g x = f (g x)
let double_incr = compose (fun x -> x + 1) (fun x -> 2*x)  // 2*x + 1
```

Of course, we can also pass named functions as arguments:

```cameligo group=lambdas
let increment x = x + 1
let double x = 2*x
let double_incr2 = compose increment double
```

</Syntax>


<Syntax syntax="jsligo">

Functions can take a function as a parameter, or return a function:
this is known as *higher-order functions*. Perhaps the most obvious
example is to define a function that takes two functions and returns a
function that is their composition, like in mathematics:

```jsligo group=lambdas
const compose = f => g => x => f (g (x));
const double_incr = compose (x => x + 1) (x => 2*x)  // 2*x + 1
```

Of course, we can also pass named functions as arguments:

```jsligo group=lambdas
const increment = x => x + 1;
const double = x => 2*x;
const double_incr2 = compose (increment) (double);
```

</Syntax>
