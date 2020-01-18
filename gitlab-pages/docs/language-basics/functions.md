---
id: functions
title: Functions
---

Writing code is fun as long as it doesn't get out of hand. To make sure our code doesn't turn into spaghetti we can group some logic into functions.

## Instruction blocks

With `block`(s) you can wrap *instructions* and *expressions* into an isolated scope.
Each `block` needs to include at least one `instruction`, or a *placeholder* instruction called `skip`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo skip
// shorthand syntax
block { skip }
// verbose syntax
begin
    skip
end
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Defining a function

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

Functions in PascaLIGO are defined using the `function` keyword followed by their `name`, `parameters` and `return` type definitions.

Here's how you define a basic function that accepts two `ints` and returns a single `int`:


```pascaligo group=a
function add(const a: int; const b: int): int is 
    begin
        const result: int = a + b;
    end with result;
```

The function body consists of two parts:

- `block {<code>}` - logic of the function
- `with <value>` - the return value of the function

#### Blockless functions

Functions that can contain all of their logic into a single instruction/expression, can be defined without the surrounding `block`.
Instead, you can inline the necessary logic directly, like this:

```pascaligo group=b
function add(const a: int; const b: int): int is a + b
```

<!--CameLIGO-->

Functions in CameLIGO are defined using the `let` keyword, like value bindings.
The difference is that after the value name a list of function parameters is provided,
along with a return type.

CameLIGO is a little different from other syntaxes when it comes to function
parameters. In OCaml, functions can only take one parameter. To get functions
with multiple arguments like we're used to in traditional programming languages,
a technique called [currying](https://en.wikipedia.org/wiki/Currying) is used.
Currying essentially translates a function with multiple arguments into a series
of single argument functions, each returning a new function accepting the next
argument until every parameter is filled. This is useful because it means that
CameLIGO can support [partial application](https://en.wikipedia.org/wiki/Partial_application).

Currying is however *not* the preferred way to pass function arguments in CameLIGO.
While this approach is faithful to the original OCaml, it's costlier in Michelson
than naive function execution accepting multiple arguments. Instead for most
functions with more than one parameter we should place the arguments in a
[tuple](language-basics/sets-lists-touples.md) and pass the tuple in as a single
parameter.

Here's how you define a basic function that accepts two `ints` and returns an `int` as well:

```cameligo group=b

let add (a,b: int * int) : int = a + b

let add_curry (a: int) (b: int) : int = a + b
```

The function body is a series of expressions, which are evaluated to give the return
value.


<!--ReasonLIGO-->

Functions in ReasonLIGO are defined using the `let` keyword, like value bindings.
The difference is that after the value name a list of function parameters is provided,
along with a return type.

Here's how you define a basic function that accepts two `ints` and returns an `int` as well:

```reasonligo group=b
let add = (a: int, b: int) : int => a + b;
```

The function body is a series of expressions, which are evaluated to give the return
value.

<!--END_DOCUSAURUS_CODE_TABS-->

## Anonymous functions

Functions without a name, also known as anonymous functions are useful in cases when you want to pass the function as an argument or assign it to a key in a record/map.

Here's how to define an anonymous function assigned to a variable `increment`, with it's appropriate function type signature.
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=c
const increment : (int -> int) = (function (const i : int) : int is i + 1);
// a = 2
const a: int = increment(1);
```

<!--CameLIGO-->
```cameligo group=c
let increment : (int -> int) = fun (i: int) -> i + 1
```

<!--ReasonLIGO-->
```reasonligo group=c
let increment: (int => int) = (i: int) => i + 1;
```

<!--END_DOCUSAURUS_CODE_TABS-->
