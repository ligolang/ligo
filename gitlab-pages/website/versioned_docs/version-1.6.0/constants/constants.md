---
id: constants
title: Constants
---

import Syntax from '@theme/Syntax';

Constants are defined by assigning the value of an expression to a
variable (that is, a value name). They are immutable by design, which
means that their values cannot be reassigned. Put in another way, they
can be assigned once, at their declaration. When defining a constant
you can also ascribe a type to it.

<Syntax syntax="cameligo">

Constant declarations are introduced by the keyword `let`, like so:

```cameligo group=constants
let a = 1
let b : int = a // Type ascription (a.k.a. annotation)
```

Note that constants can be redefined in the same scope:

```cameligo group=constants
let x = 1
let x = 2 // No error: this x shadows the previous one.
```

This is also called *shadowing*.

</Syntax>

<Syntax syntax="jsligo">

Constant declarations are introduced by the keyword `const`, like so:

```jsligo group=constants
const a = 1;
const b : int = a; // Type ascription (a.k.a. annotation)
```

Note that constants cannot be redefined in the same block scope:

```jsligo skip
const x = 1;
const x = 2; // Yields an error
```

However, the following does work:

```jsligo group=constants
const d = do {
  const x = 1;
  {
    const x = 2; // No error: a sub-block
    return x;
  }
};
```

</Syntax>
