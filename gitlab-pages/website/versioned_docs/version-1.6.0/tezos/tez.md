---
id: tez
title: tez
---

import Syntax from '@theme/Syntax';

LIGO offers some Tezos-specific data types. Here we list some of
them. Others have their own dedicated section.

The token unit on Tezos is called `tez` in LIGO. There are several
ways to write literal values of type `tez`:

  * units of millionth of `tez`, using the suffix `mutez` after a
    natural number, like `10000mutez` or `0mutez`;
  * units of `tez`, using the suffix `tz` or `tez`, like `3tz` or
    `3tez`;
  * decimal amounts of `tz` or `tez`, like `12.3tz` or `12.4tez`.

> The type is `tez`, *not* `mutez` --- which is a suffix to write
> literals.

Note that large amounts, like with numerical values of type `int` and
`nat`, can be expressed using underscores to separate groups of
digits, like `1_000mutez` (one thousand mutez) or `0.000_004tez`.

### Adding

Addition in LIGO is accomplished by means of the `+` infix
operator. Some type constraints apply, for example you cannot add a
value of type `tez` to a value of type `nat`.

In the following example you can find a series of arithmetic
operations, including various numerical types. However, some bits
remain in comments as they would otherwise not compile, for example,
adding a value of type `int` to a value of type `tez` is invalid. Note
that adding an integer to a natural number produces an integer.

<Syntax syntax="cameligo">

```cameligo group=tez
let sum : tez = 5mutez + 0.000_010tez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tez
const sum: tez = 5mutez + 1tez;
```

</Syntax>

### Subtracting

Since subtracting two amounts could result in a negative amount,
subtraction of two `tez` amounts result in an
[optional amount](../variants/options.md), like so:

<Syntax syntax="cameligo">

```cameligo group=tez
let amount : tez option = 5mutez - 1mutez (* Some (4mutez) *)
let negative : tez option = 1mutez - 5mutez (* None *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tez
const amount: option<tez> = 5mutez - 1mutez; /* Some (4mutez) */
const negative: option<tez> = 1mutez - 5mutez; /* None */
```

</Syntax>

### Multiplying

You can multiply `nat` and `tez` values:

<Syntax syntax="cameligo">

```cameligo group=tez
let mult : tez = 5n * 5mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tez
const mult: tez = 5n * 5mutez;
```

</Syntax>

### Dividing

The division of two `tez` values results into a `nat`.

<Syntax syntax="cameligo">

```cameligo group=tez
let div : nat = 10mutez / 3mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tez
const div: nat = 10mutez / 3mutez;
```

</Syntax>

## Euclidean Division

<Syntax syntax="cameligo">

For cases when you need both the quotient and the remainder, LIGO
provides the `ediv` operation. `ediv x y` returns `Some (quotient,
remainder)`, unless `y` is zero, in which case it returns `None`. The
function `ediv` is overloaded to accept tez, beyond all the
combinations of natural and integer numbers:

```cameligo group=tez_euclidean
// Some (7, 2mutez)
let ediv1 : (nat * tez) option = ediv 37mutez 5mutez

// Some (7mutez, 2mutez)
let ediv2 : (tez * tez) option = ediv 37mutez 5n
```

</Syntax>

<Syntax syntax="jsligo">

For cases when you need both the quotient and the remainder, LIGO
provides the `ediv` operation. `ediv(x,y)` returns `Some (quotient,
remainder)`, unless `y` is zero, in which case it returns `None`. The
function `ediv` is overloaded to accept tez, beyond all the
combinations of natural and integer numbers:

```jsligo group=tez_euclidean
// Some (7, 2mutez)
const ediv1: option<[nat, tez]> = ediv(37mutez, 5mutez);

// Some (7mutez, 2mutez)
const ediv2: option<[tez, tez]> = ediv(37mutez, 5n);
```

</Syntax>
