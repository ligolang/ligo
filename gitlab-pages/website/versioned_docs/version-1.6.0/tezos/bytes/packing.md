---
id: packing
title: Packing & Unpacking
---

import Syntax from '@theme/Syntax';

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
