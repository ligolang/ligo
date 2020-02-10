---
id: functions
title: Functions
---

LIGO features functions are the basic building block of contracts. For
example, entrypoints are functions.

## Declaring Functions

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

There are two ways in PascaLIGO to define functions: with or without a
*block*.

### Blocks

In PascaLIGO, *blocks* enable the sequential composition of
instructions into an isolated scope. Each block needs to include at
least one instruction.

```pascaligo skip
block { a := a + 1 }
```

If we need a placeholder, we use the instruction `skip` which leaves
the state unchanged.  The rationale for `skip` instead of a truly
empty block is that it prevents you from writing an empty block by
mistake.

```pascaligo skip
block { skip }
```

Blocks are more versatile than simply containing instructions: they
can also include *declarations* of values, like so:

```pascaligo skip
block { const a : int = 1 }
```

Functions in PascaLIGO are defined using the `function` keyword
followed by their `name`, `parameters` and `return` type definitions.

Here is how you define a basic function that computes the sum of two
integers:

```pascaligo group=a
function add (const a : int; const b : int) : int is
  block {
    const sum : int = a + b
  } with sum
```

The function body consists of two parts:

- `block { <instructions and declarations> }` is the logic of the function;
- `with <value>` is the value returned by the function.

### Blockless functions

Functions that can contain all of their logic into a single
*expression* can be defined without the need of a block:

```pascaligo
function identity (const n : int) : int is block { skip } with n  // Bad! Empty block not needed!

function identity (const n : int) : int is n  // Blockless
```

The value of the expression is implicitly returned by the
function. Another example is as follows:

```pascaligo group=b
function add (const a: int; const b : int) : int is a + b
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run-function gitlab-pages/docs/language-basics/src/functions/blockless.ligo add '(1,2)'
# Outputs: 3
```

<!--CameLIGO-->

Functions in CameLIGO are defined using the `let` keyword, like other
values. The difference is that a succession of parameters is provided
after the value name, followed by the return type. This follows OCaml
syntax. For example:
```cameligo group=c
let add (a : int) (b : int) : int = a + b
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run-function gitlab-pages/docs/language-basics/src/functions/blockless.mligo add '(1,2)'
# Outputs: 3
```

CameLIGO is a little different from other syntaxes when it comes to
function parameters. In OCaml, functions can only take one
parameter. To get functions with multiple arguments like we are used
to in imperative programming languages, a technique called
[currying](https://en.wikipedia.org/wiki/Currying) is used.  Currying
essentially translates a function with multiple arguments into a
series of single argument functions, each returning a new function
accepting the next argument until every parameter is filled. This is
useful because it means that CameLIGO supports
[partial application](https://en.wikipedia.org/wiki/Partial_application).

Currying is however *not* the preferred way to pass function arguments
in CameLIGO.  While this approach is faithful to the original OCaml,
it is costlier in Michelson than naive function execution accepting
multiple arguments. Instead, for most functions with more than one
parameter, we should gather the arguments in a
[tuple](language-basics/sets-lists-tuples.md) and pass the tuple in as
a single parameter.

Here is how you define a basic function that accepts two `ints` and
returns an `int` as well:

```cameligo group=b
let add (a, b : int * int) : int = a + b             // Uncurried
let add_curry (a : int) (b : int) : int = add (a, b) // Curried
let increment (b : int) : int = add_curry 1          // Partial application
```

You can run the `increment` function defined above using the LIGO
compiler like this:
```shell
ligo run-function gitlab-pages/docs/language-basics/src/functions/curry.mligo increment 5
# Outputs: 6
```

The function body is a single expression, whose value is returned.

<!--ReasonLIGO--> Functions in ReasonLIGO are defined using the `let`
keyword, like other values. The difference is that a tuple of
parameters is provided after the value name, with its type, then
followed by the return type.

Here is how you define a basic function that sums two integers:
```reasonligo group=b
let add = ((a, b): (int, int)) : int => a + b;
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run-function gitlab-pages/docs/language-basics/src/functions/blockless.religo add '(1,2)'
# Outputs: 3
```

The function body is a single expression, whose value is returned.
<!--END_DOCUSAURUS_CODE_TABS-->

## Anonymous functions (a.k.a. lambdas)

It is possible to define functions without assigning them a name. They
are useful when you want to pass them as arguments, or assign them to
a key in a record or a map.

Here is how to define an anonymous function:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
function increment (const b : int) : int is
   (function (const a : int) : int is a + 1) (b)

const a : int = increment (1); // a = 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo evaluate-value gitlab-pages/docs/language-basics/src/functions/anon.ligo a
# Outputs: 2
```

<!--CameLIGO-->
```cameligo group=c
let increment (b : int) : int = (fun (a : int) -> a + 1) b
let a : int = increment 1 // a = 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo evaluate-value gitlab-pages/docs/language-basics/src/functions/anon.mligo a
# Outputs: 2
```

<!--ReasonLIGO-->
```reasonligo group=c
let increment = (b : int) : int => ((a : int) : int => a + 1)(b);
let a : int = increment (1); // a == 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo evaluate-value gitlab-pages/docs/language-basics/src/functions/anon.religo a
# Outputs: 2
```

<!--END_DOCUSAURUS_CODE_TABS-->

If the example above seems contrived, here is a more common design
pattern for lambdas: to be used as parameters to functions. Consider
the use case of having a list of integers and mapping the increment
function to all its elements.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
function incr_map (const l : list (int)) : list (int) is
  list_map (function (const i : int) : int is i + 1, l)
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/functions/incr_map.ligo incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```

<!--CameLIGO-->
```cameligo group=c
let incr_map (l : int list) : int list =
  List.map (fun (i : int) -> i + 1) l
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/functions/incr_map.mligo incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```

<!--ReasonLIGO-->
```reasonligo group=c
let incr_map = (l : list (int)) : list (int) =>
  List.map ((i : int) => i + 1, l);
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/functions/incr_map.religo incr_map
"list [1;2;3]"
# Outputs: [ 2 ; 3 ; 4 ]
```


<!--END_DOCUSAURUS_CODE_TABS-->
