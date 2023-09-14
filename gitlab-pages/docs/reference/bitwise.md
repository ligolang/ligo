---
id: bitwise-reference
title: Bitwise
description: Operations on bytes
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val and : 'a -> nat -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let and : (a: &apos;a, b: nat) => nat
</SyntaxTitle>

`'a` can either be an `int` or `nat`.

It can also be used with `bytes`. But the signature might be a bit different.

`val and : bytes -> bytes -> bytes`

A bitwise `and` operation.

<Syntax syntax="cameligo">

```cameligo
let zero : nat = Bitwise.and 2n 1n
let two_bytes : bytes = Bitwise.and 0x11 0x10
```

Input
```bash
❯ ligo compile expression cameligo "Bitwise.and 0x11 0x10"
```
Output
```bash
0x10
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
let zero: nat = 2n & 1n;
let two_bytes : bytes = 0x11 & 0x10
```


Input
```bash
❯ ligo compile expression jsligo "0x11 & 0x10"
```

Output
```bash
0x10
```

</Syntax>


<SyntaxTitle syntax="cameligo">
val or :  nat -> nat -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let or: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise `or` operation.

It can also be used with `bytes`. But the signature might be a bit different.

`val or : bytes -> bytes -> bytes`

<Syntax syntax="cameligo">

```cameligo
let five : nat = Bitwise.or 4n 1n
let three_bytes : bytes = Bitwise.or 0x11 0x10
```


Input
```bash
❯ ligo compile expression cameligo "Bitwise.or 0x11 0x10"
```

Output
```bash
0x11
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
let five: nat = 4n | 1n;
let three_bytes : bytes = 0x11 | 0x10
```


Input
```bash
❯ ligo compile expression jsligo "0x11 | 0x10"
```

Output
```bash
0x11
```


</Syntax>

<SyntaxTitle syntax="cameligo">
val xor :  nat -> nat -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let xor: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise `xor` operation.

It can also be used with `bytes`. But the signature might be a bit different.

`val xor : bytes -> bytes -> bytes`

<Syntax syntax="cameligo">

```cameligo
let three : nat = Bitwise.xor 2n 1n
let one_byte : bytes = Bitwise.xor 0x11 0x10
```


Input
```bash
❯ ligo compile expression cameligo "Bitwise.xor 0x11 0x10"
```

Output
```bash
0x01
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=other
let three : nat = 2n ^ 1n;
let one_byte : bytes = 0x11 ^ 0x10
```


Input
```bash
❯ ligo compile expression jsligo "0x11 ^ 0x10"
```

Output
```bash
0x01
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val shift_left :  nat -> nat -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let shift_left: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise shift left operation.

It can also be used with `bytes`. But the signature might be a bit different.

`val shift_left : bytes -> nat -> bytes`

<Syntax syntax="cameligo">

```cameligo
let four : nat = Bitwise.shift_left 2n 1n
let five_one_two : bytes = Bitwise.shift_left 0x0100 1n
```

Input
```bash
❯ ligo compile expression cameligo "Bitwise.shift_left 0x0100 1n"
```

Output
```bash
0x000200
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
let four : nat = 2n << 1n
let five_one_two : bytes = 0x0100 << 1n
```

Input
```bash
❯ ligo compile expression jsligo "0x0100 << 1n"
```

Output
```bash
0x000200
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val shift_right :  nat -> nat -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let shift_right: (a: nat, b: nat) => nat
</SyntaxTitle>

A bitwise shift right operation.

It can also be used with `bytes`. But the signature might be a bit different.

`val shift_right : bytes -> nat -> bytes`

<Syntax syntax="cameligo">


```cameligo
let one : nat = Bitwise.shift_right 2n 1n
let zero_bytes : bytes = Bitwise.shift_right 0x01 1n
```

Input
```bash
❯ ligo compile expression cameligo "Bitwise.shift_right 0x01 1n"
```

Output
```bash
0x00
```


</Syntax>

<Syntax syntax="jsligo">

```jsligo
let one : nat = 2n >> 1n;
let zero_bytes : bytes = 0x01 >> 1n
```

Input
```bash
❯ ligo compile expression jsligo "0x01 >> 1n"
```

Output
```bash
0x00
```

</Syntax>
