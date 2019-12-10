---
id: constants-and-variables
title: Constants & Variables
---

The next building block after types are constants and variables.

## Constants

Constants are immutable by design, which means their values can't be reassigned.
When defining a constant you need to provide a `name`, `type` and a `value`:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const age : int = 25;
```

You can evaluate the constant definition above using the following CLI command:
```shell
ligo evaluate-value -s pascaligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo age
# Outputs: 25
```
<!--Cameligo-->
```cameligo
let age: int = 25
```

You can evaluate the constant definition above using the following CLI command:
```shell
ligo evaluate-value -s cameligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo age
# Outputs: 25
```

<!--Reasonligo-->
```reasonligo
let age: int = 25;
```

You can evaluate the constant definition above using the following CLI command:
```shell
ligo evaluate-value -s reasonligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.religo age
# Outputs: 25
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Variables

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

Variables, unlike constants, are mutable. They can't be used in a *global scope*, but they can be used within functions, or function arguments.

> 💡 Don't worry if you don't understand the function syntax yet. We'll get to it in upcoming sections of the docs.

> ⚠️ Please be wary that mutation only works within the function scope itself, values outside of the function scope will not be affected.



```pascaligo
// won't work, use const for global values instead
// var four: int = 4;

function add(const a: int; const b: int) : int is
    block { 
        var c : int := a + b;
     } with c
```


> ⚠️ Notice the different assignment operator `:=`

You can run the `add` function defined above using the LIGO compiler like this:

```shell
ligo run-function -s pascaligo gitlab-pages/docs/language-basics/src/variables-and-constants/add.ligo add '(1,1)' 
# Outputs: 2
```

<!--Cameligo-->

As expected from a functional language, CameLIGO uses value-binding
for variables rather than assignment. Variables are changed by replacement,
with a new value being bound in place of the old one.

> 💡 Don't worry if you don't understand the function syntax yet. We'll get to it in upcoming sections of the docs.

```cameligo

let add (a: int) (b: int) : int =
  let c : int = a + b in c
```

You can run the `add` function defined above using the LIGO compiler like this:

```shell
ligo run-function -s cameligo gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo add '(1,1)' 
# Outputs: 2
```
<!--Reasonligo-->

As expected from a functional language, Reasonligo uses value-binding
for variables rather than assignment. Variables are changed by replacement,
with a new value being bound in place of the old one.

> 💡 Don't worry if you don't understand the function syntax yet. We'll get to it in upcoming sections of the docs.

```reasonligo

let add = (a: int, b: int): int => {
  let c: int = a + b;
  c;
};
```

You can run the `add` function defined above using the LIGO compiler like this:

```shell
ligo run-function -s reasonligo gitlab-pages/docs/language-basics/src/variables-and-constants/add.religo add '(1,1)' 
# Outputs: 2
```

<!--END_DOCUSAURUS_CODE_TABS-->
