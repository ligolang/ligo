---
title: Numbers
---

import Syntax from '@theme/Syntax';

In LIGO, there are two types of numbers: integers and natural
numbers.

  * Integer literals are the same found in mainstream programming
    languages, for example, `10`, `-6` and `0`, but there is only one
    canonical zero: `0` (so, for instance, `-0` and `00` are invalid).

  * Natural numbers are written as digits followed by the suffix `n`,
    like so: `12n`, `0n`, and the same restriction on zero as integers
    applies: `0n` is the only way to specify the natural zero.

Contrary to integral numbers in other programming languages, numbers
in LIGO have arbitrary-precision, that is, they do not overflow or
underflow.

Digits of large numbers can be separated by an underscore, to increase
readability.

<Syntax syntax="cameligo">

```cameligo group=int_and_nat
// The following are integers
let zero = 0
let million = 1_000_000 // Grouping in French
let baekman = 100_0000 // Grouping in Korean

// The following are natural numbers
let zero_nat = 0n
let million_nat = 1_000_000n
let baekman_nat = 100_0000n
```

As a form of documentation, a type can be ascribed to each constant:

```cameligo group=typed_int_and_nat
let zero : int = 0
let million : int = 1_000_000
let baekman : int = 100_0000

let zero_nat : nat = 0n
let million_nat : nat = 1_000_000n
let baekman_nat : nat = 100_0000n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=int_and_nat
// The following are integers
const zero = 0
const million = 1_000_000 // Grouping in French
const baekman = 100_0000 // Grouping in Korean

// The following are natural numbers
const zero_nat = 0n
const million_nat = 1_000_000n
const baekman_nat = 100_0000n
```

As a form of documentation, a type can be ascribed to each constant:

```jsligo group=typed_int_and_nat
const zero : int = 0
const million : int = 1_000_000
const baekman : int = 100_0000

const zero_nat : nat = 0n
const million_nat : nat = 1_000_000n
const baekman_nat : nat = 100_0000n
```

</Syntax>

## Casting

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

## Adding

Addition in LIGO is accomplished by means of the `+` binary operator,
which is overloaded to apply to any combination of natural numbers and
integers, as shown in the following examples. Note that adding an
integer to a natural number produces an integer, because the compiler
cannot determine, in general, whether the result would be always a
natural number for all inputs.

<Syntax syntax="cameligo">

```cameligo group=additing
let a : int = 5 + 10    // int + int yields int
let b : nat = 5n + 10n  // nat + nat yields nat
let c : int = 5n + 10   // nat + int yields int
let d : int = 10 + 5n   // int + nat yields int
// let error : nat = 5n + 10
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=additing
const a : int = 5 + 10;    // int + int yields int
const b : nat = 5n + 10n;  // nat + nat yields nat
const c : int = 5n + 10;   // nat + int yields int
const d : int = 10 + 5n;   // int + nat yields int
// const error : nat = 5n + 10;
```

</Syntax>

## Subtracting

Subtraction in LIGO is accomplished by means of the `-` binary
operator which is overloaded to apply to any combination of natural
numbers and integers, as shown in the following examples. The rule
when subtracting two natural numbers is that the result is an integer
because, in general, the compiler cannot determine whether the value
of an expression is positive or zero for all inputs.

<Syntax syntax="cameligo">

```cameligo group=subtracting
let a : int = 5 - 10   // int - int yields int
let b : int = 5n - 2n  // nat - nat yields int
let c : int = 10n - 5  // nat - int yields int
let d : int = 5 - 10n  // int - nat yields int
// let error : nat = 5n - 2n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=subtracting
const a : int = 5 - 10;   // int - int yields int
const b : int = 5n - 2n;  // nat - nat yields int
const c : int = 10n - 5;  // nat - int yields int
const d : int = 5 - 10n;  // int - nat yields int
// const error : nat = 5n - 2n;
```

</Syntax>

## Negating

The arithmetic negation of a number is the same as subtracting that
number from zero, so the negation of a natural numbers yields an
integer:

<Syntax syntax="cameligo">

```cameligo group=negating
let a : int = -5  // - int yields int
let b : int = -5n // - nat yields int
// let error : nat = -5n
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=negating
const a : int = -5;  // - int yields int
const b : int = -5n; // - nat yields int
// const error : nat = -5n;
```
</Syntax>

## Multiplying

Multiplication in LIGO is accomplished by means of the `*` binary
operator which is overloaded to apply to any combination of natural
numbers and integers, as shown in the following examples. The type
rules for multiplication are the same as for the addition:

<Syntax syntax="cameligo">

```cameligo group=multiplying
let a : int = 5 * 10   // int * int yields int
let b : nat = 5n * 2n  // nat * nat yields nat
let c : int = 10n * 5  // nat * int yields int
let d : int = 5 * 10n  // int * nat yields int
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=multiplying
const a : int = 5 * 10;   // int * int yields int
const b : nat = 5n * 2n;  // nat * nat yields nat
const c : int = 10n * 5;  // nat * int yields int
const d : int = 5 * 10n;  // int * nat yields int
```

</Syntax>

## Dividing

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
