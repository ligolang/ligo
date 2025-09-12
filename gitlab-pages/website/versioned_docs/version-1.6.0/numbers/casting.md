---
id: casting
title: Casting
---

import Syntax from '@theme/Syntax';


In mathematics, natural numbers are a strict subset of integers, and
can be used in any context where an integer is expected. In LIGO, this
property does not hold true in general. Instead, a given binary
arithmetic operation, say, is defined four times, so it can apply to
any combination of natural numbers and integers: this is called
*overloading*, and some programming languages extend it to
user-defined functions (e.g. members in C++) -- but not LIGO.

So there are no implicit type casts in LIGO, but we can *explicitly
cast* natural numbers to integers (this is safe in all contexts where
an integer is valid) by calling the predefined function `int`. The
inverse cast, from `int` to `nat` is called in mathematics the
_absolute value_, or `abs` in LIGO.

<Syntax syntax="cameligo">

```cameligo group=casting
let one : int = int 1n // Explicit cast from nat to int
let two : nat = abs 2  // Explicit cast from int to nat
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=casting
const one : int = int(1n); // Explicit cast from nat to int
const two : nat = abs(2);  // Explicit cast from int to nat
```
</Syntax>
