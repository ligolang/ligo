---
id: bytes-reference
title: Bytes
description: Operations on bytes
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val concat : bytes -> bytes -> bytes
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let concat: (a: bytes, b: bytes) => bytes
</SyntaxTitle>


Concatenate together two `bytes` arguments and return the result.

<Syntax syntax="cameligo">

```cameligo
let concat_op (s : bytes) : bytes = Bytes.concat s 0x7070
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
let concat_op = (s: bytes): bytes => Bytes.concat(s, 0x7070);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val concats : bytes list -> bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let concats: (bs: list&lt;bytes&gt;) => bytes
</SyntaxTitle>

Concatenate together a list of `bytes` and return the result.

<SyntaxTitle syntax="cameligo">
val sub : nat -> nat -> bytes -> bytes
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let sub : (start: nat, length: nat, input: bytes) => bytes
</SyntaxTitle>

Extract bytes from `start` to `length`. For example if you gave the
input "ff7a7aff" to the following function:

<Syntax syntax="cameligo">

```cameligo
let slice_op (s : bytes) = Bytes.sub 1n 2n s
```

</Syntax>

<Syntax syntax="jsligo">

```
let slice_op = (s: bytes) => Bytes.sub(1n, 2n, s);
```

</Syntax>

It would return "7a7a".

<SyntaxTitle syntax="cameligo">
val pack : 'a -> bytes
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let pack : (data: &apos;a) => bytes
</SyntaxTitle>

Converts Michelson data structures to a binary format for serialisation.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.

<Syntax syntax="cameligo">

```cameligo
let id_string (p : string) =
  let packed : bytes = Bytes.pack p in
  Bytes.unpack packed
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=pack
function id_string (p: string) {
  let packed : bytes = Bytes.pack(p);
  return Bytes.unpack(packed);
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val unpack : bytes -> 'a option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let unpack: (serialized_data: bytes) => option&lt;&apos;a&gt;
</SyntaxTitle>

Reverses the result of using `pack` on data.

As the conversion might fail an option type is returned.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.

<Syntax syntax="cameligo">

```cameligo
let id_string (p : string) =
  let packed : bytes = Bytes.pack p in
  Bytes.unpack packed
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
function id_string (p: string) {
  let packed : bytes = Bytes.pack(p);
  return Bytes.unpack(packed);
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val length : bytes -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let length: (b: bytes) => nat
</SyntaxTitle>
