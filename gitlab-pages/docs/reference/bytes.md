---
id: bytes-reference
title: Bytes
---

## Bytes.concat(b1: bytes, b2: bytes) : bytes

Concatenate together two `bytes` arguments and return the result.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo
function concat_op (const s : bytes) : bytes is
  begin skip end with bytes_concat(s , 0x7070)
```

<!--CameLIGO-->

```cameligo
let concat_op (s : bytes) : bytes =
   Bytes.concat s 0x7070
```

<!--ReasonLIGO-->

```reasonligo
let concat_op = (s: bytes): bytes => Bytes.concat(s, 0x7070);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Bytes.slice(pos1: nat, pos2: nat, data: bytes) : bytes

Extract the bytes between `pos1` and `pos2`. **Positions are zero indexed and
inclusive**. For example if you gave the input "ff7a7aff" to the following:

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo
function slice_op (const s : bytes) : bytes is
  begin skip end with bytes_slice(1n , 2n , s)
```

<!--CameLIGO-->

```cameligo
let slice_op (s : bytes) : bytes =
   Bytes.slice 1n 2n s
```

<!--ReasonLIGO-->

```
let slice_op = (s: bytes): bytes => Bytes.slice(1n, 2n, s);
```

<!--END_DOCUSAURUS_CODE_TABS-->

It would return "7a7a" rather than "ff7a" or "ff" or "7a".

## Bytes.pack(data: a') : bytes

Converts Michelson data structures to a binary format for serialization.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(string))
```

<!--CameLIGO-->
```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

<!--ReasonLIGO-->
```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Bytes.unpack(packed: bytes) : a'

Reverses the result of using `unpack` on data, going from Michelson's binary
serialization format to the `option` type annotated on the call.

> ⚠️ `PACK` and `UNPACK` are features of Michelson that are intended to be used by people that really know what they're doing. There are several failure cases (such as `UNPACK`ing a lambda from an untrusted source), most of which are beyond the scope of this document. Don't use these functions without doing your homework first.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function id_string (const p : string) : option(string) is block {
  const packed : bytes = bytes_pack(p) ;
} with (bytes_unpack(packed): option(string))
```

<!--CameLIGO-->
```cameligo
let id_string (p: string) : string option =
  let packed: bytes = Bytes.pack p in
  ((Bytes.unpack packed): string option)
```

<!--ReasonLIGO-->
```reasonligo
let id_string = (p: string) : option(string) => {
  let packed : bytes = Bytes.pack(p);
  ((Bytes.unpack(packed)): option(string));
};
```

<!--END_DOCUSAURUS_CODE_TABS-->
