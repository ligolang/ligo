---
id: subtracting
title: Subtracting
---


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
