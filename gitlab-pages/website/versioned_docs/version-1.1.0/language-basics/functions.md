---
id: functions
title: Functions
---

import Syntax from '@theme/Syntax';

LIGO functions are the basic building block of contracts. For example,
entrypoints are functions and each smart contract needs a main
function that dispatches control to the entrypoints (it is not already
the default entrypoint).

The semantics of function calls in LIGO is that of a *copy of the
arguments but also of the environment*. In the case of JsLIGO, this
means that any mutation (assignment) on variables outside the scope of
the function will be lost when the function returns, just as the
mutations inside the functions will be.

## Declaring Functions

<Syntax syntax="cameligo">

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
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/functions/blockless.mligo \
  'add 1 2'
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
[tuple](sets-lists-tuples.md) and pass the tuple in as
a single parameter.

Here is how you define a basic function that accepts two integers and
returns an integer as well:

```cameligo group=b
let add (a, b : int * int) : int = a + b             // Uncurried
let add_curry (a : int) (b : int) : int = add (a, b) // Curried
let increment : int -> int = add_curry 1             // Partial application
```

You can run the `increment` function defined above using the LIGO
compiler like this:
```shell
ligo run evaluate-call \
  gitlab-pages/docs/language-basics/src/functions/curry.mligo \
  increment 5
# Outputs: 6
```

The function body is a single expression, whose value is returned.

By default, LIGO will warn about unused arguments inside
functions. In case we do not use an argument, we can use the wildcard
`_` to prevent warnings. Either use `_` instead of the argument
identifier:

```cameligo
let k (x : int) (_ : int) = x
```

or use an identifier starting with wildcard:

```cameligo
let k (x : int) (_y : int) = x
```


Sometimes, one has to chain multiple function applications.
In this case, parentheses are needed.

```cameligo group=revapp
let f (x : int) = x + 1
let g (x : int) = x - 2
let h (x : int) = x + x - 3

(* Here we apply function f on value 42,
   then apply g on the result,
   and then apply h on the result *)
let result = h (g (f 42))

(* Parentheses are indeed necessary here. If we remove them, we have : *)
// let result' = h g f 42
(* Which is different, it is equivalent to : *)
// let result' = ((h g) f) 42
```

Here, one might want to reduce the number of parentheses, for readability.
In this case, the reverse-application operator (`|>`) can be used instead.

Expression `f x` can be rewritten as `x |> f`,
and `g (f x)` can be rewritten as `x |> f |> g`
(you can think of it as "I take `x`, give it to function `f`, and then the result to function `g`").

Above `result` can thus be rewritten as :

```cameligo group=revapp
let result = 42 |> f |> g |> h
```

Function application has precedence over reverse-application operator,
which means `f 42 |> g` is the same as `(f 42) |> g` and not `f (42 |> g)`.
So you can rewrite `result` as :

```cameligo group=revapp
let result = f 42 |> g |> h
```

This can be useful when you have to deal with a long chain of function calls.

This operator actually comes from [OCaml's pervasives](https://v2.ocaml.org/releases/4.02/htmlman/libref/Pervasives.html#6_Compositionoperators).
Other similar operators will be added when enabling support for custom operator definition.

</Syntax>

<Syntax syntax="jsligo">

Functions in JsLIGO can be defined in two main ways: using the keyword
`function` or `const` (the keyword `let` is defaulted to `const` in
this instance). The latter manner is preferred when the function body
is an expression. For example, here is how you define a basic function
that sums two integers:

```jsligo group=b
const add = (a: int, b: int) => a + b;
```

You can call the function `add` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/functions/blockless.jsligo \
  'add(1,2)'
# Outputs: 3
```

If the body contains statements instead of a single expression, you
would use a block and a `return` statement:

```jsligo group=b
const myFun = (x: int, y: int) => {
  const doubleX = x + x;
  const doubleY = y + y;
  return doubleX + doubleY;
};
```

although it is arguably more readable to use `function`, like so:

```jsligo group=b
function myFun2 (x: int, y: int) {
  const doubleX = x + x;
  const doubleY = y + y;
  return doubleX + doubleY;
}
```

Note that JsLIGO, like JavaScript, requires the `return` keyword to indicate
what is being returned. If `return` is not used, it will be the same as
`return unit`.

By default, LIGO will warn about unused arguments inside
functions. In case we do not use an argument, its name should start with
`_` to prevent warnings.

```jsligo
const k_other = (x: int, _y: int) => x;
```

</Syntax>


## Anonymous functions (a.k.a. lambdas)

It is possible to define functions without assigning them a name. They
are useful when you want to pass them as arguments, or assign them to
a key in a record or a map.

Here is how to define an anonymous function:

<Syntax syntax="cameligo">

```cameligo group=c
let increment (b : int) : int = (fun (a : int) -> a + 1) b
let a = increment 1 // a = 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/functions/anon.mligo a
# Outputs: 2
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
const increment = (b) => ((a) => a + 1) (b);
const a = increment(1); // a == 2
```

You can check the value of `a` defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/functions/anon.jsligo a
# Outputs: 2
```

</Syntax>


If the example above seems contrived, here is a more common design
pattern for lambdas: to be used as parameters to functions. Consider
the use case of having a list of integers and mapping the increment
function to all its elements.

<Syntax syntax="cameligo">

```cameligo group=c
let incr_map (l : int list) : int list =
  List.map (fun (i : int) -> i + 1) l
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call \
  gitlab-pages/docs/language-basics/src/functions/incr_map.mligo \
  incr_map "[1;2;3]"
# Outputs: CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))), equivalent to [ 2 ; 3 ; 4 ]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
let incr_map = l => List.map(i => i + 1, l);
```
You can call the function `incr_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call \
  gitlab-pages/docs/language-basics/src/functions/incr_map.jsligo \
  incr_map "list([1,2,3])"
# Outputs: CONS(2 , CONS(3 , CONS(4 , LIST_EMPTY()))), equivalent to list([ 2 , 3 , 4 ])
```

</Syntax>


## Nested functions (also known as closures)

It is possible to define a functions inside another function. These
functions have access to variables in the same scope.

<Syntax syntax="cameligo">

```cameligo
let closure_example (i : int) : int =
  let closure = fun (j : int) -> i + j in
  closure i
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
function closure_example (i) {
  let closure = j => i + j;
  return closure(i);
};
```

</Syntax>


## Recursive functions

<Syntax syntax="cameligo">
In CameLIGO, recursive functions are defined using the `rec` keyword

```cameligo group=d
let rec sum (n, acc : int * int) : int =
  if n < 1 then acc else sum (n-1, acc + n)

let rec fibo (n, n_1, n_0 : int * int * int) : int =
  if n < 2 then n_1 else fibo (n-1, n_1 + n_0, n_1)
```
</Syntax>

<Syntax syntax="jsligo">

In JsLigo, recursive functions are defined and called using the same syntax as non-recursive functions.

```jsligo group=d
function sum (n: int, acc: int): int {
  if (n < 1) return acc else return sum(n-1, acc + n);
};

function fibo (n: int, n_1: int, n_0: int): int {
  if (n < 2) return n_1 else return fibo (n-1, n_1 + n_0, n_1);
};
```
</Syntax>

<!-- updated use of entry -->