---
id: variables
title: Variables and constants
---

import Syntax from '@theme/Syntax';

LIGO features mutable variables, that is, variables whose values can be reassigned.
By contrast, constants can be assigned only once.

## Variables

<Syntax syntax="cameligo">

To declare a variable, use the keyword `let` (as with constants) followed by the keyword `mut`.
The initial assignment uses `=`, but subsequent assignments use `:=`, as in this example:

```cameligo group=mutating
let add (a, b : int * int) : int =
  let mut c = a + b in // Mutable c is assigned a + b
  let () = c := c + 1  // Reassignment of incremented c
  in c                 // c = a + b + 1
```

</Syntax>

<Syntax syntax="jsligo">

To declare a variable, use the keyword `let` instead of the keyword `const`, which is for constants.
To assign new values to the variable, use the `=` operator, as in this example:

```jsligo group=mutating
function add (a: int, b:int) : int {
  let c = a + b; // not const!
  c++;           // Reassignment of incremented c
  return c;      // c == a + b + 1
};
```

</Syntax>

### Silent variables

The compiler warns you when you declare a variable but do not use it.
To ignore a variable, turn it into a silent variable by prefixing its name with an underscore or using an underscore as its name.

For example, LIGO entrypoints receive a parameter and the current value of storage as arguments.
If the entrypoint code doesn't access one or both of these arguments, prefix the argument name with an underscore to hide the compiler warning, as in this example:

<Syntax syntax="cameligo">

```cameligo group=silent_variables
[@entry] let reset (_param: unit) (_storage : int) : operation list * int = [], 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=silent_variables
@entry
const reset = (_param : unit, _storage : int) : [list<operation>, int] => [[], 0];
```

</Syntax>

## Constants

Constant values cannot be reassigned after they are declared.

<Syntax syntax="cameligo">

To declare a constant, use the keyword `let`, as in this example:

```cameligo group=constants
let a = 1
let b : int = a // Type ascription (a.k.a. annotation)
```

If you assign a new value to a constant of the same name, LIGO creates a new constant in a new scope.
This is also called *shadowing*, based on the OCaml concept.

```cameligo group=constants
let x = 1
let x = 2 // No error: this x shadows the previous one.
```

</Syntax>

<Syntax syntax="jsligo">

To declare a constant, use the keyword `const`, as in this example:

```jsligo group=constants
const a = 1;
const b : int = a; // Type ascription (a.k.a. annotation)
```

You cannot assign a new value to a constant in the same scope:

```jsligo skip
const x = 1;
const x = 2; // Yields an error
```

However, the following example works because the constants are in different scopes:

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
