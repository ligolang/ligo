---
id: bytes-reference
title: Bytes — Manipulate bytes data
---

import Syntax from '@theme/Syntax';

## Bytes.concat(b1: bytes, b2: bytes) : bytes

Concatenate together two `bytes` arguments and return the result.

<Syntax syntax="pascaligo">

```pascaligo
function concat_op (const s : bytes) : bytes is
  begin skip end with bytes_concat(s , 0x7070)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let concat_op (s : bytes) : bytes =
   Bytes.concat s 0x7070
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let concat_op = (s: bytes): bytes => Bytes.concat(s, 0x7070);
```

</Syntax>

## Bytes.slice(pos1: nat, pos2: nat, data: bytes) : bytes

Extract the bytes between `pos1` and `pos2`. **Positions are zero indexed and
inclusive**. For example if you gave the input "ff7a7aff" to the following:



<Syntax syntax="pascaligo">

```pascaligo
function slice_op (const s : bytes) : bytes is
  begin skip end with bytes_slice(1n , 2n , s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let slice_op (s : bytes) : bytes =
   Bytes.slice 1n 2n s
```

</Syntax>
<Syntax syntax="reasonligo">

```
let slice_op = (s: bytes): bytes => Bytes.slice(1n, 2n, s);
```

</Syntax>


It would return "7a7a" rather than "ff7a" or "ff" or "7a".

## Bytes.pack(data: a') : bytes

Converts Michelson data structures to a binary format for serialization.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.



<Syntax syntax="pascaligo">

```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(string))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

</Syntax>


## Bytes.unpack(packed: bytes) : a'

Reverses the result of using `unpack` on data, going from Michelson's binary
serialization format to the `option` type annotated on the call.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.



<Syntax syntax="pascaligo">

```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(string))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

</Syntax>

