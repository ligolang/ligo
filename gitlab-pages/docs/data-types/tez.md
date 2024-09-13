---
id: tez
title: tez
---

import Syntax from '@theme/Syntax';

The token unit on Tezos is called `tez` in LIGO. There are several
ways to write literal values of type `tez`:

  * Units of millionths of `tez`, using the suffix `mutez` after a
    natural number, like `10000mutez` or `0mutez`
  * Units of `tez`, using the suffix `tz` or `tez`, like `3tz` or
    `3tez`
  * Decimal amounts of `tz` or `tez`, like `12.3tz` or `12.4tez`

> LIGO uses the type `tez`, *not* `mutez`.
> The suffix `mutez` is only for writing literal amounts of millionths of tez.

Like integers and nats, you can express large amounts of tez with underscores to separate groups of
digits, like `1_000mutez` (one thousand mutez) or `0.000_004tez`.

## Adding

You can add amounts of tez with the `+` operator, as in the following example.
You cannot add amounts of tez to integers or nats.

<Syntax syntax="cameligo">

```cameligo group=add
let sum : tez = 5mutez + 0.000_010tez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=add
const sum: tez = 5mutez + 1tez;
```

</Syntax>

## Subtracting

Because subtracting two amounts could result in a negative amount,
subtracting two `tez` amounts results in an
[optional amount](.././data-types/variants#options), as in these examples:

<Syntax syntax="cameligo">

```cameligo group=subtract
let amount : tez option = 5mutez - 1mutez (* Some (4mutez) *)
let negative : tez option = 1mutez - 5mutez (* None *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=subtract
const amount: option<tez> = 5mutez - 1mutez; /* Some (4mutez) */
const negative: option<tez> = 1mutez - 5mutez; /* None */
```

</Syntax>

## Multiplying

You can multiply `nat` and `tez` values:

<Syntax syntax="cameligo">

```cameligo group=multiply
let mult : tez = 5n * 5mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=multiply
const mult: tez = 5n * 5mutez;
```

</Syntax>

## Dividing

Because LIGO features neither floating-point nor fixed-point
arithmetic, division in LIGO is Euclidean.
Dividing two `tez` values returns the quotient of the operation as a nat.

<Syntax syntax="cameligo">

```cameligo group=divide
let div : nat = 10mutez / 3mutez
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=divide
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
