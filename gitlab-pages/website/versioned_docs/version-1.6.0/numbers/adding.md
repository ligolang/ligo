---
id: adding
title: Adding
---

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
