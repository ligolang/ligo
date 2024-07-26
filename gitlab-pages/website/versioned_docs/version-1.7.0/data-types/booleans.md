---
id: booleans
title: Booleans
---

import Syntax from '@theme/Syntax';

The predefined type `bool` has exactly two values: `true` and `false`.

<Syntax syntax="cameligo">

```cameligo group=booleans
let a : bool = true
let b : bool = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=booleans
const a: bool = true;
const b: bool = false;
```

</Syntax>

## Or

<Syntax syntax="cameligo">

The logical disjunction ("or") is implemented by the binary operator
`||`:


```cameligo group=or
let or_1 : bool = false || true  // true
let or_2 : bool = false || false // false
let or_3 : bool = true  || true  // true
let or_4 : bool = true  || false // true
```

Note that you can also use the keyword `or` instead of the symbol `||`
(as in OCaml):

```cameligo group=or
let or_1 : bool = false or true  // true
let or_2 : bool = false or false // false
let or_3 : bool = true  or true  // true
let or_4 : bool = true  or false // true
```

</Syntax>

<Syntax syntax="jsligo">

The logical disjunction ("or") is implemented by the binary operator
`||`.

```jsligo group=or
const or_1: bool = false || true;  // true
const or_2: bool = false || false; // false
const or_3: bool = true  || true;  // true
const or_4: bool = true  || false; // true
```

</Syntax>

## And

The logical conjunction ("and") is implemented by the binary operator
`&&`.

<Syntax syntax="cameligo">

```cameligo group=conjunction
let and_1 : bool = false && true  // false
let and_2 : bool = false && false // false
let and_3 : bool = true  && true  // true
let and_4 : bool = true  && false // false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=conjunction
const and_1: bool = false && true;  // false
const and_2: bool = false && false; // false
const and_3: bool = true  && true;  // true
const and_4: bool = true  && false; // false
```

</Syntax>

## Not

<Syntax syntax="cameligo">

The logical negation ("not") is implemented by the unary operator
`not`.

```cameligo group=not
let not_1 : bool = not true  // false
let not_2 : bool = not false // true
```

</Syntax>

<Syntax syntax="jsligo">

The logical negation ("not") is implemented by the unary operator
`!`.

```jsligo group=not
const not_1: bool = !true  // false
const not_2: bool = !false // true
```

</Syntax>

## Comparing

Boolean values are the result of comparisons of values. Numbers and
strings are completely ordered. Booleans can be compared for
equality. Two values need to be of the same type to be compared, but
not all values of the same type can be compared: only those with <em>comparable types</em> (a concept directly lifted from Michelson)
such as `int`, `nat`, `string`, and `bool` itself. The comparison
operators are overloaded so they are defined on all comparable types.

<Syntax syntax="cameligo">

```cameligo group=comparing
let a : bool = 1 = 1   // equality (true)
let b : bool = 1 <> 0  // inequality (true)
let c : bool = 1 > 0   // greater than (true)
let d : bool = 0 < 1   // lower than (true)
let e : bool = 0 >= 0  // greater than or equal (true)
let f : bool = 0 <= 0  // lower than or equal (true)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=comparing
const a: bool = 1 == 1;  // equality (true)
const b: bool = 1 != 0;  // inequality (true)
const c: bool = 1 > 0;   // greater than (true)
const d: bool = 0 < 1;   // lower than (true)
const e: bool = 0 >= 0;  // greater than or equal (true)
const f: bool = 0 <= 0;  // lower than or equal (true)
```

</Syntax>

## Conditional expressions

Conditional logic enables forking the control flow depending on the
state, that is, the values available at a given point in the code. Put
in a less technical manner, conditionals enable decision making.

A conditional expression is made of three parts:
<ol>
  <li> a condition, that is, a boolean expression;</li>
  <li> an expression evaluated if, and only if, the condition is true;</li>
  <li> an expression evaluated if, and only if, the condition is false.</li>
</ol>

<Syntax syntax="cameligo">

The syntax uses the keywords `if`, `then` and `else` to separate the
three parts, like so:

```cameligo group=conditionals
let a = 0
let b = 1
let min = if a < b then a else b // min = 0
```

</Syntax>

<Syntax syntax="jsligo">

The syntax uses a ternary operator with the symbols `?` and `:` to
separate the three parts:

```jsligo group=conditionals
const a = 0;
const b = 1;
const min = (a < b) ? a : b; // min == 0
```

Note: Parentheses are often necessary before `?`, but not always: you
can either rely on the compiler error message or always use
parentheses.

</Syntax>
