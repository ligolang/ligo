---
id: multiplying
title: Multiplying
---

import Syntax from '@theme/Syntax';

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
