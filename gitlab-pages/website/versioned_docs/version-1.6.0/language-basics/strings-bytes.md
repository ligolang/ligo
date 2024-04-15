---
id: strings-bytes
title: Strings & Bytes
---

import Syntax from '@theme/Syntax';

## Strings

Strings are defined using the built-in `string` type like this:

<Syntax syntax="cameligo">

```
let a : string = "Hello Alice"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
const a = "Hello Alice";
```

</Syntax>

### Concatenating Strings

<Syntax syntax="cameligo">

Strings can be concatenated using the `^` operator.

```cameligo group=a
let name : string = "Alice"
let greeting : string = "Hello"
let full_greeting : string = greeting ^ " " ^ name
```

</Syntax>

<Syntax syntax="jsligo">

Strings can be concatenated using the `+` operator.

```jsligo group=a
const name = "Alice";
const greeting = "Hello";
const full_greeting = greeting + " " + name;
```

</Syntax>

### Extracting Substrings

Substrings can be extracted using the predefined function
`String.sub`. The first character has index 0 and the interval of
indices for the substring has inclusive bounds.

<Syntax syntax="cameligo">

```cameligo group=b
let name  : string = "Alice"
let slice : string = String.sub 0n 1n name (* slice = "A" *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const name = "Alice";
const slice = String.sub (0n, 1n, name); // slice == "A"
```

</Syntax>

> ⚠️ Notice that the offset and length of the slice are natural
> numbers.

### Length of Strings

The length of a string can be found using a built-in function:

<Syntax syntax="cameligo">

```cameligo group=c
let name : string = "Alice"
let length : nat = String.length name  // length = 5
```

> Note that `String.size` is *deprecated*.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
const name = "Alice";
const length = String.length(name);  // length == 5
```

</Syntax>

## Bytes

Byte literals are defined using the prefix `0x` followed by hexadecimal digits like this:

<Syntax syntax="cameligo">

```cameligo
let b : bytes = 0x7070
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
const b = 0x7070;
```

</Syntax>

Moreover, a string literal can be converted to its bytes representation:

<Syntax syntax="cameligo">

```cameligo
let bs : bytes = [%bytes "foo"]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
const bs = (bytes `foo`);
```

</Syntax>


### Concatenating Bytes

Bytes can be concatenated using the `Bytes.concat` function.

<Syntax syntax="cameligo">

```cameligo group=d
let white : bytes = 0xffff
let black : bytes = 0x0000
let pixels : bytes = Bytes.concat white black (* 0xffff0000 *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=d
const white = 0xffff;
const black = 0x0000;
const pixels = Bytes.concat(white, black); // 0xffff0000
```

</Syntax>

### Extracting Bytes

Bytes can be extracted using the predefined function `Bytes.sub`.  The
first parameter takes the start index and the second parameter takes
the number of bytes. Pay special attention to how `bytes` are
indexed.

<Syntax syntax="cameligo">

```cameligo group=e
let b     : bytes = 0x12345678
let slice : bytes = Bytes.sub 1n 2n b (* 0x3456 *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=e
const b     = 0x12345678;
const slice = Bytes.sub (1n, 2n, b); // 0x3456
```

</Syntax>

### Length of Bytes

The length of `bytes` can be found using a built-in function `Bytes.length`:

<Syntax syntax="cameligo">

```cameligo group=f
let b      : bytes = 0x123456
let length : nat   = Bytes.length b  (* length = 3 *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=f
const b      = 0x123456;
const length = Bytes.length(b);  // length = 3
```

</Syntax>

### Bitwise operators

You can perform bitwise operation on `bytes` as follows:

<Syntax syntax="cameligo">

```cameligo group=g
(* Bitwise and *)
let b_and         = 0x0005 land 0x0106 (* 0x0004 *)

(* Bitwise or *)
let b_or          = 0x0005 lor  0x0106 (* 0x0107 *)

(* Bitwise xor *)
let b_xor         = 0x0005 lxor 0x0106 (* 0x0103 *)

(* Bitwise shift left *)
let b_shift_left  = 0x06   lsl  8n     (* 0x0600 *)

(* Bitwise shift right *)
let b_shift_right = 0x0006 lsr  1n     (* 0x0003 *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=g
/* Bitwise and */
const b_and           =  0x0005 & 0x0106; // 0x0004

/* Bitwise or */
const b_or            = 0x0005 | 0x0106; // 0x0107

/* Bitwise xor */
const b_xor           = 0x0005 ^ 0x0106; // 0x0103

/* Bitwise shift left */
const b_shift_left    = 0x06 << 8n; // 0x0600

/* Bitwise shift right */
const b_shift_right   = 0x0006 >> 1n; // 0x0003
```

</Syntax>


### From `bytes` to `nat` and back

You can case `bytes` to `nat` using the built-in `nat` function and vice-versa
using using the `bytes` built-in function.

<Syntax syntax="cameligo">

```cameligo group=h
(* bytes -> nat *)
let test_bytes_nat = nat 0x1234 (* 1234n *)

(* nat -> bytes *)
let test_nat_bytes = bytes 4660n (* 0x1234 *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=h
/* bytes -> nat */
const test_bytes_nat = nat(0x1234) // 1234n

/* nat -> bytes */
const test_nat_bytes = bytes(4660n) // 0x1234
```

</Syntax>

### From `bytes` to `int` and back

You can cast `bytes` to `int` using the built-in `int` function and
vice-versa using the `bytes` built-in function.

<Syntax syntax="cameligo">

```cameligo group=h
(* bytes -> int *)
let test_bytes_int = int 0x1234 (* 4660 *)

(* int -> bytes *)
let test_int_bytes = bytes 4660 (* 0x1234 *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=h
/* bytes -> int */
const test_bytes_int = int(0x1234) // 4660

/* int -> bytes */
const test_int_bytes = bytes(4660) // 0x1234
```

</Syntax>

<!-- updated use of entry -->