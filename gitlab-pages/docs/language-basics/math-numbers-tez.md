---
id: math-numbers-tez
title: Math, Numbers & Tez
---

LIGO offers three built-in numerical types: `int`, `nat` and
`tez`. Values of type `int` are integers; values of type `nat` are
natural numbers (integral numbers greater than or equal to zero);
values of type `tez` are units of measure of Tezos tokens.

  * Integer literals are the same found in mainstream programming
    languages, for example, `10`, `-6` and `0`, but there is only one
    canonical zero: `0` (so, for instance, `-0` and `00` are invalid).

  * Natural numbers are written as digits follwed by the suffix `n`,
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

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
// int + int yields int
const a : int = 5 + 10

// nat + int yields int
const b : int = 5n + 10

// tez + tez yields tez
const c : tez = 5mutez + 0.000_010tez

//tez + int or tez + nat is invalid
// const d : tez = 5mutez + 10n

// two nats yield a nat
const e : nat = 5n + 10n

// nat + int yields an int: invalid
// const f : nat = 5n + 10;

const g : int = 1_000_000
```

> Pro tip: you can use underscores for readability when defining large
> numbers:
>
>```pascaligo
> const sum : tez = 100_000mutez
>```

<!--CameLIGO-->
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

> Pro tip: you can use underscores for readability when defining large
> numbers:
>
>```cameligo
>let sum : tez = 100_000mutez
>```

<!--ReasonLIGO-->
```reasonligo group=a
// int + int yields int
let a : int = 5 + 10;

// nat + int yields int
let b : int = 5n + 10;

// tez + tez yields tez
let c : tez = 5mutez + 0.000_010tez;

// tez + int or tez + nat is invalid:
// let d : tez = 5mutez + 10n;

// two nats yield a nat
let e : nat = 5n + 10n;

// nat + int yields an int: invalid
// let f : nat = 5n + 10;

let g : int = 1_000_000;
```

> Pro tip: you can use underscores for readability when defining large
> numbers:
>```reasonligo
>let sum : tex = 100_000mutez;
>```
<!--END_DOCUSAURUS_CODE_TABS-->

## Subtraction

Subtraction looks as follows.

> ⚠️ Even when subtracting two `nats`, the result is an `int`

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=b
const a : int = 5 - 10

// Subtraction of two nats yields an int
const b : int = 5n - 2n

// Therefore the following is invalid
// const c : nat = 5n - 2n

const d : tez = 5mutez - 1mutez
```

<!--CameLIGO-->
```cameligo group=b
let a : int = 5 - 10

// Subtraction of two nats yields an int
let b : int = 5n - 2n

// Therefore the following is invalid
// let c : nat = 5n - 2n

let d : tez = 5mutez - 1mutez
```

<!--ReasonLIGO-->
```reasonligo group=b
let a : int = 5 - 10;

// Subtraction of two nats yields an int
let b : int = 5n - 2n;

// Therefore the following is invalid
// let c : nat = 5n - 2n;

let d : tez = 5mutez - 1mutez;
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Multiplication

You can multiply values of the same type, such as:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

```pascaligo group=c
const a : int = 5 * 5
const b : nat = 5n * 5n

// You can also multiply `nat` and `tez`
const c : tez = 5n * 5mutez
```

<!--CameLIGO-->
```cameligo group=c
let a : int = 5 * 5
let b : nat = 5n * 5n

// You can also multiply `nat` and `tez`
let c : tez = 5n * 5mutez
```

<!--ReasonLIGO-->
```reasonligo group=c
let a : int = 5 * 5;
let b : nat = 5n * 5n;

// You can also multiply `nat` and `tez`
let c : tez = 5n * 5mutez;
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Euclidean Division

In LIGO you can divide `int`, `nat`, and `tez`. Here is how:

> ⚠️ Division of two `tez` values results into a `nat`

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=d
const a : int = 10 / 3
const b : nat = 10n / 3n
const c : nat = 10mutez / 3mutez
```

<!--CameLIGO-->
```cameligo group=d
let a : int = 10 / 3
let b : nat = 10n / 3n
let c : nat = 10mutez / 3mutez
```

<!--ReasonLIGO-->
```reasonligo group=d
let a : int = 10 / 3;
let b : nat = 10n / 3n;
let c : nat = 10mutez / 3mutez;
```

<!--END_DOCUSAURUS_CODE_TABS-->

LIGO also allows you to compute the remainder of the Euclidean
division. In LIGO, it is a natural number.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=d
const a : int = 120
const b : int = 9
const rem1 : nat = a mod b  // 3
const c : nat = 120n
const rem2 : nat = c mod b  // 3
const d : nat = 9n
const rem3 : nat = c mod d  // 3
const rem4 : nat = a mod d  // 3
```

<!--CameLIGO-->
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

<!--ReasonLIGO-->
```reasonligo group=d
let a : int = 120;
let b : int = 9;
let rem1 : nat = a mod b;  // 3
let c : nat = 120n;
let rem2 : nat = c mod b;  // 3
let d : nat = 9n;
let rem3 : nat = c mod d;  // 3
let rem4 : nat = a mod d;  // 3
```

## From `int` to `nat` and back

You can *cast* an `int` to a `nat` and vice versa. Here is how:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=e
const a : int = int (1n)
const b : nat = abs (1)
```

<!--CameLIGO-->
```cameligo group=e
let a : int = int (1n)
let b : nat = abs (1)
```

<!--ReasonLIGO-->
```reasonligo group=e
let a : int = int (1n);
let b : nat = abs (1);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Checking a `nat`

You can check if a value is a `nat` by using a predefined cast
function which accepts an `int` and returns an optional `nat`: if the
result is not `None`, then the provided integer was indeed a natural
number, and not otherwise.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=e
const is_a_nat : option (nat) = is_nat (1)
```

<!--CameLIGO-->
```cameligo group=e
let is_a_nat : nat option = Michelson.is_nat (1)
```

<!--ReasonLIGO-->
```reasonligo group=e
let is_a_nat : option (nat) = Michelson.is_nat (1);
```

<!--END_DOCUSAURUS_CODE_TABS-->
