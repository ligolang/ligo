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

<Syntax syntax="cameligo">

```cameligo group=a
let age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo age
# Outputs: 25
```

</Syntax>

<Syntax syntax="jsligo">

> Constants in JsLIGO are enforced:

```jsligo skip
const x = do {
  const age = 25;
  age = 3; // Yields an error
};
```

Unlike the other syntaxes, JsLIGO doesn't allow variable names to be reused in the same block scope:

```jsligo skip
const x = () => {
  const age = 25;
  const age = 3; // Yields an error
};
```

However, the following does work:

```jsligo group=d
const x = () => {
  const _age = 25;
  {
    const _age = 3; // does not give an error
    return _age;
  }
};
```

You can evaluate a constant definition using the following CLI
command:

```jsligo
const age : int = 25;
```

```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.jsligo age
# Outputs: 25
```

</Syntax>


## Variables

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
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo 'add 1 1'
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
  return c;
}
```

You can run the `add` function defined above using the LIGO compiler
like this:

```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/add.jsligo 'add(1, 1)'
# Outputs: 2
```

</Syntax>

## Escaped Identifiers

Both variables and constants are, at the level of the lexicon,
_identifiers_. Each flavour of LIGO has its own set of
keywords. Sometimes we need an identifier that is the same as a
keyword, or, perhaps, we do not want to shadow a predefined
identifier, like `amount`. In those cases, you could suffix your
identifier with an underscore, like `amount_`. (Beware that if you
prefix with an underscore, like `_amount`, the compiler will not
complain about the value being not used.) But this is not a good
practice because we do not pronounce aloud the underscores, and there
is the issue of one or two underscores. To solve all those problems,
in LIGO, you can prefix you identifier with `@`, like `@amount`.

<Syntax syntax="cameligo">

```cameligo group=a
let @Unique_name = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
const @Unique_name = true
```

</Syntax>

<!-- updated use of entry -->