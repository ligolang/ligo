---
id: functions
title: Functions
---

import Syntax from '@theme/Syntax';

LIGO functions are the basic building blocks of contracts.
Contracts can have any number of functions.

Entrypoints are a special case of functions.
Outside consumers can call only the functions that are designated as entrypoints.
Entrypoints must also follow certain rules that do not apply to functions in general.
See [Entrypoints](./contracts/entrypoints).

<Syntax syntax="cameligo">

To declare a function, use the `let` keyword, just like declaring variables.
The function declaration includes its parameters, after the function name, following OCaml syntax, as in this example:

```cameligo group=fun_decl
let add x y = x + y
let int_add (x : int) (y : int) : int = x + y
```

To call a function, use its name and pass the correct number and type of parameters, as in this example:

```cameligo group=fun_decl
let five_plus_three = int_add 5 3
```

By default, LIGO warns about unused parameters inside functions.
If the function does not use one of its parameters, you can prefix its name with an underscore to hide the warning, as in this example:

```cameligo group=unused
let drop (x : int) (_y : int) : int = x // _y silently ignored
```

</Syntax>

<Syntax syntax="jsligo">

As in JavaScript, you can declare functions with the `function` keyword or the `const` or `let` keywords.

Here are some examples:

```jsligo group=sum
function add_function (x: int, y: int): int {
  const z = x + y;
  return z;
}

const add_function_const = (x: int, y: int): int  => {
  const z = x + y;
  return z;
}
```

This type of function must use the `return` keyword to set the return value.
If it does not use `return`, the function returns unit by default.

Also as in JavaScript, when you declare functions with the `const` or `let` keywords, you can replace the body of the function with a single expression and omit the `return` keyword.
The `add_expression` function in the following example is equivalent to the `add_function` and `add_function_const` functions in the previous example:

```jsligo group=sum
const add_expression = (x, y) => x + y;  // Single-expression body
```

To call a function, use its name and pass the correct number and type of parameters, as in this example:

```jsligo group=sum
const five_plus_three = add_expression(5, 3);
```

By default, LIGO warns about unused parameters inside functions.
If the function does not use one of its parameters, you can prefix its name with an underscore to hide the warning, as in this example:

```jsligo group=unused
const drop = (x, _y) => x; // _y silently ignored
```

</Syntax>

## Polymorphic functions

Polymorphic functions can take advantage of parametric types to accept a wide variety of inputs instead of only a single input type.
For more information, see [Polymorphic functions](../data-types/parametric_types#polymorphic-functions).

## Scope and side effects

<Syntax syntax="cameligo">

Functions can access, or _capture_, immutable variables and functions that are outside their code as long as those variables and functions are in their scope.
For example, a function can capture another function in its scope so it can call it, as in this example:

```cameligo skip
let double_incr (x : int) : int => add(x, x) + 1 // add is captured from outside
```

Capturing also works with nested functions just like two functions at the same scope level:

```cameligo skip
let convoluted_doubling (x : int) : int =
  let addX y = x + y in
  addX(x)
```

Functions cannot change values outside their scope, but they can define variables within their scope that have the same name.
In this example, a function appears to change the value of a parameter, but it is actually defining a new variable within the scope of the function:

```cameligo skip
let addValue (value : int) : int =
  let value = 12 in
  value + 5
```

</Syntax>

<Syntax syntax="jsligo">

Functions can access, or _capture_, immutable variables and functions that are outside their code as long as those variables and functions are in their scope.
For example, a function can capture another function in its scope so it can call it, as in this example:

```jsligo group=sum
const double_incr = x => add_expression (x, x) + 1; // add_expression is captured from outside
```

Capturing also works with nested functions just like two functions at the same scope level:

```jsligo skip
function convoluted_doubling (x) {
  const addX = y => x + y;  // x is bound by convoluted_doubling
  return addX(x);
}
```

Functions cannot change values outside their scope, including values passed as parameters.
In this example, a function tries to change a variable and a parameter but fails:

```jsligo skip
let myConstant = 5;
const addConstant = (x) => {
  myConstant++; // Mutable variable "myConstant" not found.
  x += myConstant; // Mutable variable "x" not found.
  return x + myConstant;
}
```

</Syntax>

## Currying functions

To simplify a function for use in different contexts, you can create new functions that decompose the original function into simpler functions that take fewer parameters.
Creating these simplified functions is called [currying](https://en.wikipedia.org/wiki/Currying), and it enables [partial application](https://en.wikipedia.org/wiki/Partial_application).

For example, this code creates a function that accepts a single parameter, a tuple that contains two integers.
Then it creates a curried function that accepts two separate integers as parameters and passes them to the first function as a tuple.
Then it uses partial application to create a function that accepts one parameter and passes it to the curried function:

<Syntax syntax="cameligo">

```cameligo group=curry
let add (x, y) = x + y         // Uncurried
let add_curry x y = add (x, y) // Curried
let increment = add_curry 1    // Partial application
let one = increment 0
```

In this example, type of `increment` is `int -> int`, meaning that it is a function that takes an argument of the type `int` and returns a value of the type `int`.
Similarly, the type of `add_curry` is `int -> int -> int`,  which means `int -> (int -> int)` and demonstrates how currying can partially apply it (only on its first argument) with the declaration of `increment`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=curry
const add = ([x, y]) => x + y;           // Uncurried
const add_curry = (x, y) => add([x, y]); // Curried
const increment = add_curry(1);          // Partial application
const one = increment(0);
```

</Syntax>

## Function expressions

Sometimes you need to use a function only once, so it doesn't need a name.
Also, giving the function a name might incur a slight risk of cluttering the scope or being captured unintentionally.
Functions without names are called *anonymous functions*, *function expressions*, or *lambdas*.

<Syntax syntax="cameligo">

Function expressions are introduced with the `fun` keyword, followed by the parameters, then an arrow `->`, followed by the function body (an expression):

```cameligo group=anonymous
let sum = fun x y -> x + y      // Uncurried function expression
let add = fun (x,y) -> x + y    // Curried function expression
let increment = fun x -> x + 1
```

The anonymous functions `fun x y -> x + y` and `fun (x,y) -> x + y` are expressions, and you can use them *without a name* in contexts where functions of type `int -> int -> int`, respectively `int * int -> int`, are valid.

</Syntax>

<Syntax syntax="jsligo">

Function expressions are called *arrow functions* in JsLIGO:

```jsligo group=anonymous
const sum = (x,y) => x + y;
const increment = x => x + 1; // Or (x) => x + 1
```

Note that when there is a single parameter that is not given a type, the parentheses are not necessary, but they are if the return type is given, as in this example:

```jsligo group=anonymous
const decrement = (x) : int => x - 1;
```

The anonymous function `(x,y) => x + y` is an expression, and you can use it *without a name* in contexts where functions of type `(x: int, y: int) => int` are valid.

Note: When a function takes **one** argument that is a tuple, parentheses are mandatory, as in this example:

```jsligo group=anonymous
const comp_sum = ([x,y]) => x + y;
```

That function is different from the function `(x,y) => x + y`, which takes **two** arguments.
In other words, `sum` has type `(x: int, y: int) => int` and `comp_sum` has type `([x,y] : [int,int]) => int`.

</Syntax>

## Recursion

<Syntax syntax="cameligo">

As in OCaml, recursive functions are defined using the `let rec`
keywords:

```cameligo group=recursion
let rec sum (n, acc : int * int) : int =
  if n < 1 then acc else sum (n-1, acc + n)

let rec fibonacci (n, n_1, n_0 : int * int * int) : int =
  if n < 2 then n_1 else fibonacci (n-1, n_1 + n_0, n_1)
```
</Syntax>

<Syntax syntax="jsligo">

Recursive functions are defined and called using the same syntax as non-recursive functions.

```jsligo group=recursion
function sum (n: int, acc: int) : int {
  if (n < 1) return acc else return sum (n-1, acc + n);
};

function fibonacci (n: int, n_1: int, n_0: int): int {
  if (n < 2) return n_1 else return fibonacci (n-1, n_1 + n_0, n_1);
};
```

This means that all values, including functions, declared in the same block or top-level scope, must have different names, because they can all potentially be mutually recursive.

</Syntax>

## Higher-order functions

<Syntax syntax="cameligo">

Functions that take a function as a parameter or return a function are known as *higher-order functions*.
This example accepts two functions and composes them into one, as in mathematics:

```cameligo group=lambdas
let compose f g x = f (g x)
let double_incr = compose (fun x -> x + 1) (fun x -> 2*x)  // 2*x + 1
```

You can also pass named functions as arguments:

```cameligo group=lambdas
let increment x = x + 1
let double x = 2*x
let double_incr2 = compose increment double
```

</Syntax>

<Syntax syntax="jsligo">

Functions that take a function as a parameter or return a function are known as *higher-order functions*.
This example accepts two functions and composes them into one, as in mathematics:

```jsligo group=lambdas
const compose = f => g => x => f (g (x));
const double_incr = compose (x => x + 1) (x => 2*x)  // 2*x + 1
```

You can also pass named functions as arguments:

```jsligo group=lambdas
const increment = x => x + 1;
const double = x => 2*x;
const double_incr2 = compose (increment) (double);
```

</Syntax>

## Inlining

Inlining is the process of embedding the code of a function instead of storing the function as a separate block of code.
To save space, the LIGO compiler inlines function code rather than using a separate function when the function is used only once and is pure, which means it has no side effects.
In this context, side effects include creating operations or exceptions.

The compiler does not inline functions that are used more than once or are not pure because doing so often results in larger contracts.
However, in some cases, you may want to force the compiler to inline function code.
You can force the compiler to inline functions that are used more than once, but you cannot force the compiler to inline functions that cause side effects.

<Syntax syntax="cameligo">

To force inlining, use the `[@inline]` attribute.

```cameligo group=inlining
[@inline]
let fst (p : nat * nat) = p.0

[@entry]
let main (p : nat * nat) (s : nat * nat) : operation list * (nat * nat) =
  [], (fst (p.0, p.1), fst (s.1, s.0))
```

You can measure the difference between inlining and not inlining with the `info measure-contract` command.
In this case, inlining the function saves space because the function is so small.
This table shows the results of the command `ligo info measure-contract inline.mligo` with and without the `[@inline]` attribute:

</Syntax>

<Syntax syntax="jsligo">

To force inlining, use the `@inline` decorator.

```jsligo group=inlining
@inline
const fst = (p: [nat, nat]) => p[0];

@entry
const main = (p: [nat, nat], s: [nat, nat]) : [list<operation>, [nat, nat]] =>
    [[], [fst([p[0], p[1]]), fst([s[1], s[0]])]];
```

You can measure the difference between inlining and not inlining with the `info measure-contract` command.
In this case, inlining the function saves space because the function is so small.
This table shows the results of the command `ligo info measure-contract inline.jsligo` with and without the `@inline` decorator:

</Syntax>

<table>
    <tr>
        <td>With inlining</td><td>46 bytes</td>
    </tr>
    <tr>
        <td>Without inlining</td><td>97 bytes</td>
    </tr>
</table>

:::info
Note that these results can change due to ongoing work to optimise output of
the LIGO compiler.
:::
