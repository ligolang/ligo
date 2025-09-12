---
id: bytes
title: Sequences of bytes
---

import Syntax from '@theme/Syntax';

Bytes are used for serializing data, for example to compute signature
hashes. Conversely, they can be used to deserialise external data, in
which case the expected LIGO type needs to be specified.

### Literals

Byte literals are sequences of bytes (eight-bit values, also known as
_octets_), defined using the prefix `0x` followed by hexadecimal
digits, or none if the denoted literal is zero:

<Syntax syntax="cameligo">

```cameligo group=bytes
let a : bytes = 0x70FF
let zero : bytes = 0x
let zero_too = 0x00
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bytes
const a : bytes = 0x70FF;
const zero : bytes = 0x;
const zero_too = 0x00;
```

</Syntax>

Clearly, this means that literal bytes are always comprised of an even
number of hexadecimal digits (because one hexadecimal digit requires
up to four bits in binary, and eight are needed to make up a byte).

### From numbers to bytes and back

Some other numerals can be converted to bytes by means of calling the
predefined function `bytes`, which is overloaded. The reverse
conversion is done by the predefined functions `int` and `nat`. For
instance, here how to create bytes from natural numbers and integers:

<Syntax syntax="cameligo">

```cameligo group=bytes
let b : bytes = bytes 123n   // 7B in hexadecimal
let c : bytes = bytes 123
let d : bytes = bytes (-123) // Two's complement

let n : nat = nat 0x7B // n = 123n
let i : int = int 0x7B // i = 123
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bytes
const b: bytes = bytes(123n); // 7B in hexadecimal
const c: bytes = bytes(123);
const d: bytes = bytes(-123); // Two's complement

const n: nat = nat(0x7B); // n == 123n
const i: int = int(0x7B); // i == 123
```

</Syntax>

> Note: See
> [Two's complement](https://en.wikipedia.org/wiki/Two's_complement).

### From strings

A string literal can be converted to bytes in two ways:

  1. by interpreting the [ASCII](https://en.wikipedia.org/wiki/ASCII)
code of each character (which spans over two hexadecimal digits) as
one byte;
  2. by interpreting directly each character as one hexadecimal digit.


In the former case, the syntax is somewhat odd -- as opposed to simply
calling the function `bytes`:

<Syntax syntax="cameligo">

```cameligo group=bytes
let from_ascii : bytes = [%bytes "foo"]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bytes
const from_ascii: bytes = bytes`foo`; // Not a call
```

</Syntax>

The latter case is implemented as a type cast:

<Syntax syntax="cameligo">

```cameligo group=bytes
// raw = from_ascii
let raw : bytes = ("666f6f" : bytes)
```

> Note that both the `[%bytes ...]` and `(... : bytes)` syntaxes apply
> only to *string literals*, not general expressions of type
> `string`. In other words, the contents of the strings must be
> available in-place at compile-time. (This actually reveals that
> `("666f6f" : bytes)` is not really a cast, as casts are
> non-operations.)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bytes
// raw == from_ascii
const raw: bytes = ("666f6f" as bytes);
```

> Note that both syntaxes apply respectively only to *verbatim* string
> literals and general strings, not general expressions of type
> `string`. In other words, the contents of the strings must be
> available at compile-time. (This actually reveals that `("666f6f" as
> bytes)` is not really a cast, as casts are non-operations.)

</Syntax>
