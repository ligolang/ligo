---
id: bytes
title: Bytes
---

import Syntax from '@theme/Syntax';

Bytes are used for serializing data, for example to compute signature
hashes. Conversely, they can be used to deserialise external data, in
which case the expected LIGO type needs to be specified.

## Literals

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

## From numbers to bytes and back

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

## From strings

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
const raw: bytes = "666f6f";
```

> Note that both syntaxes apply respectively only to *verbatim* string
> literals and general strings, not general expressions of type
> `string`. In other words, the contents of the strings must be
> available at compile-time. (This actually reveals that `("666f6f" as
> bytes)` is not really a cast, as casts are non-operations.)

</Syntax>

## Concatenating

Two or more bytes can be concatenated.

<Syntax syntax="cameligo">

```cameligo group=concatenating
let two : bytes = Bytes.concat 0x70 0xAA
let three : bytes = Bytes.concats [0x70; 0xAA; 0xFF]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=concatenating
const two: bytes = Bytes.concat(0x70, 0xAA);
const three: bytes = Bytes.concats([0x70, 0xAA, 0xFF]);
```

</Syntax>

## Sizing

In order to obtain the length of a sequence of bytes, use the
predefined function `Bytes.length` like so:

<Syntax syntax="cameligo">

```cameligo group=sizing
let len : nat = Bytes.length 0x0AFF // len = 2n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sizing
const len: nat = Bytes.length(0x0AFF); // len == 2n
```

</Syntax>

## Slicing

Bytes can be extracted using the predefined function `Bytes.sub`. The
first parameter is the start index and the second is the number of
bytes of the slice we want. Keep in mind that the first byte in a
sequence has index `0n`.

<Syntax syntax="cameligo">

```cameligo group=slicing
let large = 0x12345678
let slice = Bytes.sub 1n 2n large // sub = 0x3456
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=slicing
const large = 0x12345678;
const slice = Bytes.sub(1n, 2n, large); // sub == 0x3456
```

</Syntax>

## Bitwise operations

The bitwise operations on sequences of bytes are as follows:

<Syntax syntax="cameligo">

```cameligo group=bitwise
// Bitwise "and"
let and : bytes = 0x0005 land 0x0106 // 0x0004

// Bitwise "or"
let @or : bytes = 0x0005 lor 0x0106 // 0x0107

// Bitwise "xor"
let xor : bytes = 0x0005 lxor 0x0106 // 0x0103

// Bitwise "shift left"
let shift_left : bytes = 0x06 lsl 8n // 0x0600

// Bitwise "shift right"
let shift_right : bytes = 0x0006 lsr 1n // 0x0003
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=bitwise
// Bitwise "and"
const and: bytes = 0x0005 & 0x0106; // 0x0004

// Bitwise "or"
const or: bytes = 0x0005 | 0x0106; // 0x0107

// Bitwise "xor"
const xor: bytes = 0x0005 ^ 0x0106; // 0x0103

// Bitwise "shift left"
const shift_left: bytes = 0x06 << 8n; // 0x0600

// Bitwise "shift right"
const shift_right: bytes = 0x0006 >> 1n; // 0x0003
```

</Syntax>

## Packing and unpacking

As Michelson provides the instructions `PACK` and `UNPACK` for data
serialisation, so does LIGO with `Bytes.pack` and `Bytes.unpack`.  The
former serialises Michelson data structures into a binary format, and
the latter reverses that transformation. Unpacking may fail, so the
return type of `Byte.unpack` is an option that needs to be annotated.

> Note: `PACK` and `UNPACK` are Michelson instructions that are
> intended to be used by people that really know what they are
> doing. There are several risks and failure cases, such as unpacking
> a lambda from an untrusted source or casting the result to the wrong
> type. Be careful.


<Syntax syntax="cameligo">

```cameligo group=packing
let id_string (p : string) : string option =
  let packed: bytes = Bytes.pack p
  in Bytes.unpack packed
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=packing
const id_string = (p: string) : option<string> => {
  let packed = Bytes.pack(p);
  return Bytes.unpack(packed);
};
```

</Syntax>

## Cryptography

One common use of bytes, beyond packing and unpacking, is
cryptography. The predefined module `Crypto` provides the following
hashing functions, which are efficient because they are natively
supported by the Michelson virtual machine:

<Syntax syntax="cameligo">

```cameligo skip
val blake2b : bytes -> bytes
val sha256 : bytes -> bytes
val sha512 : bytes -> bytes
val sha3 : bytes -> bytes
val keccak : bytes -> bytes
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const blake2b: bytes => bytes;
const sha256: bytes => bytes;
const sha512: bytes => bytes;
const sha3: bytes => bytes;
const keccak: bytes => bytes;
```

</Syntax>
