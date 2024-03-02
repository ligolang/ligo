---
id: math-numbers-tez
title: Math, Numbers & Tez
---

import Syntax from '@theme/Syntax';

LIGO offers three built-in numerical types: `int`, `nat` and
`tez`. Values of type `int` are integers; values of type `nat` are
natural numbers (integral numbers greater than or equal to zero);
values of type `tez` are units of measure of Tezos tokens.

  * Integer literals are the same found in mainstream programming
    languages, for example, `10`, `-6` and `0`, but there is only one
    canonical zero: `0` (so, for instance, `-0` and `00` are invalid).

  * Natural numbers are written as digits followed by the suffix `n`,
    like so: `12n`, `0n`, and the same restriction on zero as integers
    applies: `0n` is the only way to specify the natural zero.
  * Tezos tokens can be specified using literals of three kinds:
      * units of millionth of `tez`, using the suffix `mutez` after a
        natural literal, like `10000mutez` or `0mutez`;
      * units of `tez`, using the suffix `tz` or `tez`, like `3tz` or
        `3tez`;
      * decimal amounts of `tz` or `tez`, like `12.3tz` or `12.4tez`.

Note that large integral values can be expressed using underscores to
separate groups of digits, like `1_000mutez` or `0.000_004tez`.

## Addition

Addition in LIGO is accomplished by means of the `+` infix
operator. Some type constraints apply, for example you cannot add a
value of type `tez` to a value of type `nat`.

In the following example you can find a series of arithmetic
operations, including various numerical types. However, some bits
remain in comments as they would otherwise not compile, for example,
adding a value of type `int` to a value of type `tez` is invalid. Note
that adding an integer to a natural number produces an integer.

<Syntax syntax="cameligo">

```cameligo group=a
// int + int yields int
let a : int = 5 + 10

// nat + int yields int
let b : int = 5n + 10

// tez + tez yields tez
let c : tez = 5mutez + 0.000_010tez

// tez + int or tez + nat is invalid
// let d : tez = 5mutez + 10n

// two nats yield a nat
let e : nat = 5n + 10n

// nat + int yields an int: invalid
// let f : nat = 5n + 10

let g : int = 1_000_000
```

> Tip: you can use underscores for readability when defining large
> numbers:
>
>```cameligo
>let sum : tez = 100_000mutez
>```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
// int + int yields int
const a = 5 + 10;

// nat + int yields int
const b = 5n + 10;

// tez + tez yields tez
const c: tez = 5mutez + 1tez;

// tez + int or tez + nat is invalid:
// const d : tez = 5mutez + 10n;

// two nats yield a nat
const e: nat = 5n + 10n;

// nat + int yields an int: invalid
// const f : nat = 5n + 10;

const g = 1_000_000;
```

> Tip: you can use underscores for readability when defining large
> numbers:
>```jsligo
>let sum : tez = 100_000mutez;
>```

</Syntax>

## Subtraction

Subtraction looks as follows.

> ⚠️ Even when subtracting two `nats`, the result is an `int`.

<Syntax syntax="cameligo">

```cameligo group=b
let a : int = 5 - 10

// Subtraction of two nats yields an int
let b : int = 5n - 2n

// Therefore the following is invalid
// let c : nat = 5n - 2n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const a = 5 - 10;

// Subtraction of two nats yields an int
const b: int = 5n - 2n;

// Therefore the following is invalid
// const c : nat = 5n - 2n;
```

</Syntax>

From protocol *`Ithaca`* onwards subtracting values of type `tez`
yeilds an optional value (due to the Michelson instruction
`SUB_MUTEZ`)

<Syntax syntax="cameligo">

```cameligo group=b
let d : tez option = 5mutez - 1mutez (* Some (4mutez) *)
let e : tez option = 1mutez - 5mutez (* None *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const d : option<tez> = 5mutez - 1mutez; /* Some (4mutez) */
const e : option<tez> = 1mutez - 5mutez; /* None */
```

</Syntax>

## Multiplication

You can multiply values of the same type, such as:

<Syntax syntax="cameligo">

```cameligo group=c
let a : int = 5 * 5
let b : nat = 5n * 5n

// You can also multiply `nat` and `tez`
let c : tez = 5n * 5mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
const a = 5 * 5;
const b: nat = 5n * 5n;

// You can also multiply `nat` and `tez`
const c: tez = 5n * 5mutez;
```

</Syntax>


## Euclidean Division

In LIGO you can divide `int`, `nat`, and `tez`. Here is how:

> ⚠️ Division of two `tez` values results into a `nat`.

<Syntax syntax="cameligo">

```cameligo group=d
let a : int = 10 / 3
let b : nat = 10n / 3n
let c : nat = 10mutez / 3mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=d
const a: int = 10 / 3;
const b: nat = 10n / 3n;
const c: nat = 10mutez / 3mutez;
```

</Syntax>


LIGO also allows you to compute the remainder of the Euclidean
division. In LIGO, it is a natural number.

<Syntax syntax="cameligo">

```cameligo group=d
let a : int = 120
let b : int = 9
let rem1 : nat = a mod b  // 3
let c : nat = 120n
let rem2 : nat = c mod b  // 3
let d : nat = 9n
let rem3 : nat = c mod d  // 3
let rem4 : nat = a mod d  // 3
```

</Syntax>

<Syntax syntax="jsligo">

> The behaviour of the `%` operator in JsLIGO is different from JavaScript.
> In JsLIGO, `%` is a modulus operator and in JavaScript it's a remainder
> operator. In the case of positive numbers everything is the same, but
> not with negative numbers.

```jsligo group=e
const a = 120;
const b = 9;
const rem1 = a % b;  // 3
const c = 120n;
const rem2 = c % b;  // 3
const d = 9n;
const rem3 = c % d;  // 3
const rem4 = a % d;  // 3
```

</Syntax>


For cases when you need both the quotient and the remainder, LIGO provides the
`ediv` operation. `ediv x y` returns `Some (quotient, remainder)`, unless `y`
is zero, in which case it returns `None`

<Syntax syntax="cameligo">

```cameligo group=f
let a : int = 37
let b : int = 5
let ediv1 : (int * nat) option = ediv a b  // Some (7, 2)
let c : nat = 37n
let ediv2 : (int * nat) option = ediv c b  // Some (7, 2)
let d : nat = 5n
let ediv3 : (nat * nat) option = ediv c d  // Some (7, 2)
let ediv4 : (int * nat) option = ediv a d  // Some (7, 2)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=f
const a = 37;
const b = 5;
const ediv1 : option<[int , nat]> = ediv(a, b);  // Some (7, 2)
const c = 37n;
const ediv2: option<[int , nat]> = ediv(c, b);  // Some (7, 2)
const d = 5n;
const ediv3: option<[nat , nat]> = ediv(c, d);  // Some (7, 2)
const ediv4: option<[int , nat]> = ediv(a, d);  // Some (7, 2)
```

</Syntax>


## From `int` to `nat` and back

You can *cast* an `int` to a `nat` and vice versa. Here is how:

<Syntax syntax="cameligo">

```cameligo group=e
let a : int = int (1n)
let b : nat = abs (1)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=g
const a = int(1n);
const b = abs(1);
```

</Syntax>


## Checking a `nat`

You can check if a value is a `nat` by using a predefined cast
function which accepts an `int` and returns an optional `nat`: if the
result is not `None`, then the provided integer was indeed a natural
number, and not otherwise.

<Syntax syntax="cameligo">

```cameligo group=e
let is_a_nat : nat option = is_nat (1)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=h
const is_a_nat = is_nat(1);
```

</Syntax>

<Syntax syntax="cameligo">

## Bitwise operations
You can perform bitwise operations as follows:

> bitwise operations can be performed mostly with `nat`'s
>
> only in case of bitwise and, the first operand can be either `int` or `nat`

```cameligo
// Bitwise and (first operand can be int or nat)
let four : nat = 4n land 4n // 4
let four_ : nat = 7 land 4n // 4
// Bitwise or
let seven : nat = 7n lor 4n // 7
// Bitwise xor
let three : nat = 7n lxor 4n // 3
// Bitwise shift left
let fourteen : nat = 7n lsl 1n // 14
// Bitwise shift right
let seven_ : nat = 14n land 1n // 7
```

</Syntax>

<Syntax syntax="jsligo">

## Increment operator

Increment opeator increments (adds one to) the value of the binder.

In the **prefix** position (`++p`) the operator increments the value and returns
the latest incremented value.

In the **postfix** position (`p++`) the operator increments the value but
returns the old value before the increment.

```jsligo test-ligo group=increment_ops

const testInc = (() => {
  let inc = 0;

  // Prefix increment operator
  assert(++inc == 1);
  assert(inc   == 1);

  // Postfix increment operator
  assert(inc++ == 1);
  assert(inc   == 2);
})();

```


## Decrement operator

Decrement opeator decrements (subtracts one from) the value of the binder.

In the **prefix** position (`--p`) the operator decrements the value and returns
the latest decremented value.

In the **postfix** position (`p--`) the operator decrements the value but
returns the old value before the decrement.

```jsligo test-ligo group=decrement_ops

const testDec = (() => {
  let v = 10;

  // Prefix decrement operator
  assert(--v == 9);
  assert(v   == 9);

  // Postfix decrement operator
  assert(v-- == 9);
  assert(v   == 8);
})();

```

</Syntax>

<!-- updated use of entry -->