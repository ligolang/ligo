---
id: mutating
title: Mutating
---

import Syntax from '@theme/Syntax';

In _imperative programming_, the state is modified in place by
instructions, instead of new versions of values being constructed. A
typical example of imperative programming is loops.

LIGO features mutable variables, that is, variables whose values can
be reassigned --- contrary to constants, which can be only assigned
once.

<Syntax syntax="cameligo">

The declaration of mutable variables start with the usual keyword
`let` (as constants do), but followed by the keyword `mut`. The
initial assignment uses `=`, but subsequent assignments use `:=`, like
so:

```cameligo group=mutating
let add (a, b : int * int) : int =
  let mut c = a + b in // Mutable c is assigned a + b
  let () = c := c + 1  // Reassignment of incremented c
  in c                 // c = a + b + 1
```

</Syntax>

<Syntax syntax="jsligo">

The declaration of mutable variables start with the keyword `let`,
instead of `const` for constants. All assignments use the `=`
operator, like so:

```jsligo group=mutating
function add (a: int, b:int) : int {
  let c = a + b; // not const!
  c++;           // Reassignment of incremented c
  return c;      // c == a + b + 1
};
```

</Syntax>
