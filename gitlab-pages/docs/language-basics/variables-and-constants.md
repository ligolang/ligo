---
id: constants-and-variables
title: Constants & Variables
---

import Syntax from '@theme/Syntax';

The next building block after types are *constants* and *variables*.

## Constants

Constants are immutable by design, which means their values cannot be
reassigned. Put in another way, they can be assigned once, at their
declaration. When defining a constant you need to provide a `name`,
`type` and a `value`:


<Syntax syntax="pascaligo">

```pascaligo group=a
const age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo --entry-point age
# Outputs: 25
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo --entry-point age
# Outputs: 25
```

</Syntax>

<Syntax syntax="jsligo">

> Constants in JsLIGO are enforced:

```jsligo skip
let x = (a : int) : int => {
  const age = 25;
  age = 3; // Yields an error
};
```

Unlike the other syntaxes, JsLIGO doesn't allow variable names to be reused in the same block scope:

```jsligo skip
let x = a => {
  const age = 25;
  const age = 3; // Yields an error
};
```

However, the following does work:

```jsligo group=d
let x = a => {
  const age = 25;
  {
   const age = 3; // does not give an error
   return age;
  }
};
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.jsligo --entry-point age
# Outputs: 25
```

</Syntax>


## Variables


<Syntax syntax="pascaligo">

Variables, unlike constants, are *mutable*. They cannot be declared in
a *global scope*, but they can be declared and used within functions,
or as function parameters.

> ⚠️ Please be wary that mutation only works within the function scope
> itself, values outside of the function scope will not be
> affected. In other words, when a function is called, its arguments
> are copied, *as well as the environment*. Any side-effect to that
> environment is therefore lost when the function returns.


```pascaligo group=b
// The following is invalid: use `const` for global values instead.
// var four := 4

function add (const a : int; const b : int) is {
  var c := a + 2 * b;
  c := c - b
} with c
```

> ⚠ Notice the assignment operator `:=` for `var`, instead of `=` for
> constants.

You can run the `add` function defined above using the LIGO compiler
like this:

```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.ligo '(1,1)' --entry-point add
# Outputs: 2
```

</Syntax>
<Syntax syntax="cameligo">

As expected in the pure subset of a functional language, CameLIGO only
features *constant values*: once they are declared, the value cannot
be changed (or "mutated").

```cameligo group=c
let add (a, b : int * int) =
  let c = a + b in c
```

You can run the `add` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo '(1,1)' --entry-point add
# Outputs: 2
```

</Syntax>

<Syntax syntax="jsligo">

Variables, unlike constants, are *mutable*.

> ⚠️ Please be wary that mutation only works within the function scope
> itself, values outside of the function scope will not be
> affected. In other words, when a function is called, its arguments
> are copied, *as well as the environment*. Any side-effect to that
> environment is therefore lost when the function returns.


```jsligo group=b
let add = (a: int, b: int): int => {
  let c = a;
  c = c + b;
  return c
}
```

You can run the `add` function defined above using the LIGO compiler
like this:

```shell
ligo run evaluate-call gitlab-pages/docs/language-basics/src/variables-and-constants/add.jsligo '(1,1)' --entry-point add
# Outputs: 2
```

</Syntax>

## Escaped Identifiers

Both variables and constants are, at the level of the lexicon,
_identifiers_. Each flavour of LIGO has its own set of keywords,
PascaLIGO with the most. Sometimes we need an identifier that is the
same as a keyword, or, perhaps, we do not want to shadow a predefined
identifier, like `amount`. In those cases, you could suffix your
identifier with an underscore, like `amount_`. (Beware that if you
prefix with an underscore, like `_amount`, the compiler will not
complain about the value being not used.) But this is not a good
practice because we do not pronounce aloud the underscores, and there
is the issue of one or two underscores. To solve all those problems,
in LIGO, you can prefix you identifier with `@`, like `@amount`.

<Syntax syntax="pascaligo">

```pascaligo group=a
const @Unique_name = true
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=a
let @Unique_name = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
let @Unique_name = true
```

</Syntax>
