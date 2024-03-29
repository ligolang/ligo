---
id: declaring
title: Declaring
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
underflow. (See [Tezos-specific features](../language-basics/tezos-specific.md) for more).

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
