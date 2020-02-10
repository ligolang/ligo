---
id: constants-and-variables
title: Constants & Variables
---

The next building block after types are *constants* and *variables*.

## Constants

Constants are immutable by design, which means their values cannot be
reassigned. Put in another way, they can be assigned once, at their
declaration. When defining a constant you need to provide a `name`,
`type` and a `value`:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
const age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo evaluate-value gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo age
# Outputs: 25
```
<!--CameLIGO-->
```cameligo group=a
let age : int = 25
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo evaluate-value gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo age
# Outputs: 25
```

<!--ReasonLIGO-->
```reasonligo group=a
let age : int = 25;
```

You can evaluate the constant definition above using the following CLI
command:
```shell
ligo evaluate-value gitlab-pages/docs/language-basics/src/variables-and-constants/const.religo age
# Outputs: 25
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Variables

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

Variables, unlike constants, are *mutable*. They cannot be declared in
a *global scope*, but they can be declared and used within functions,
or as function parameters.

> ⚠️ Please be wary that mutation only works within the function scope
> itself, values outside of the function scope will not be affected.


```pascaligo group=b
// The following is invalid: use `const` for global values instead.
// var four : int := 4

function add (const a : int; const b : int) : int is
  block {
    var c : int := a + b
  } with c
```

> ⚠️ Notice the assignment operator `:=` for `var`, instead of `=` for
> constants.

You can run the `add` function defined above using the LIGO compiler
like this:

```shell
ligo run-function gitlab-pages/docs/language-basics/src/variables-and-constants/add.ligo add '(1,1)'
# Outputs: 2
```

<!--CameLIGO-->

As expected in the pure subset of a functional language, CameLIGO only
features *constant values*: once they are declared, the value cannot
be changed (or "mutated").

```cameligo group=c
let add (a : int) (b : int) : int =
  let c : int = a + b in c
```

You can run the `add` function defined above using the LIGO compiler
like this:
```shell
ligo run-function gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo add '(1,1)'
# Outputs: 2
```
<!--ReasonLIGO-->

As expected in the pure subset of a functional language, ReasonLIGO
only features *constant values*: once they are declared, the value
cannot be changed (or "mutated").

```reasonligo group=c
let add = ((a, b): (int, int)): int => {
  let c : int = a + b;
  c;
};
```

You can run the `add` function defined above using the LIGO compiler
like this:
```shell
ligo run-function gitlab-pages/docs/language-basics/src/variables-and-constants/add.religo add '(1,1)'
# Outputs: 2
```

<!--END_DOCUSAURUS_CODE_TABS-->
