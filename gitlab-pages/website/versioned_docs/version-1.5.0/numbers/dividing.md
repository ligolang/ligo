---
id: dividing
title: Dividing
---

import Syntax from '@theme/Syntax';

Because LIGO features neither floating-point nor fixed-point
arithmetic, division in LIGO is Euclidean. The predefined binary
operator `/` returns the quotient and is overloaded like the
multiplication. Of course, division by zero triggers an exception that
interrups the execution, so the programmer must make sure this case
cannot happen because the compiler cannot determine, in general, if a
variable will have a given value (or not) for all inputs.

<Syntax syntax="cameligo">

```cameligo group=dividing
let a : int = 10 / 3    // int / int yields int
let b : nat = 10n / 3n  // nat / nat yields nat
let c : int = 10n / 3   // nat / int yields int
let d : int = 10 / 3n   // int / nat yields int
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=dividing
const a : int = 10 / 3;    // int / int yields int
const b : nat = 10n / 3n;  // nat / nat yields nat
const c : int = 10n / 3;   // nat / int yields int
const d : int = 10 / 3n;   // int / nat yields int
```

</Syntax>

<Syntax syntax="cameligo">

The binary operator `mod` returns the <em>positive modulo</em> of the
Euclidean division, that is, the following holds:

> (n*(a/n)+(a%n) == a) && (0n <= a % n) && (a % n < abs(n))

It is overloaded as the Euclidean division `/` to allow for
all four combinations of natural numbers and integers.

```cameligo group=mod
let a : nat = 120  mod 9  // int mod int yields nat
let b : nat = 120n mod 9  // nat mod int yields nat
let b : nat = 120n mod 9n // nat mod nat yields nat
let c : nat = 120  mod 9n // int mod nat yields nat
```
</Syntax>

<Syntax syntax="jsligo">

The binary operator `%` returns the <em>positive modulo</em> of the
Euclidean division, that is, the following holds:

> (n*(a/n)+(a%n) == a) && (0n <= a % n) && (a % n < abs(n))

It is overloaded as the Euclidean division `/` to allow for all four
combinations of natural numbers and integers.

```jsligo group=mod
const a : nat = 120  % 9;  // int % int yields nat
const b : nat = 120n % 9;  // nat % int yields nat
const c : nat = 120n % 9n; // nat % nat yields nat
const d : nat = 120  % 9n; // int % nat yields nat
```

</Syntax>

> It is possible to obtain both the quotient and remainder together, by
> means of the predefined function `ediv`: See [optional values](../variants/options.md).
