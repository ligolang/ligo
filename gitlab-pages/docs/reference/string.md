---
id: string-reference
title: String — Manipulate string data
---

import Syntax from '@theme/Syntax';

## String.size(s: string) : nat

Get the size of a string. [Michelson only supports ASCII strings](http://tezos.gitlab.io/whitedoc/michelson.html#constants) 
so for now you can assume that each character takes one byte of storage.



<Syntax syntax="pascaligo">

```pascaligo
function string_size (const s: string) : nat is size(s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let size_op (s: string) : nat = String.size s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let size_op = (s: string): nat => String.size(s);
```

</Syntax>


## String.length(s: string) : nat

Alias for `String.size`.

## String.slice(pos1: nat, pos2: nat, s: string) : string

Get the substring of `s` between `pos1` inclusive and `pos2` inclusive. For example
the string "tata" given to the function below would return "at".


<Syntax syntax="pascaligo">

```pascaligo
function slice_op (const s : string) : string is string_slice(1n , 2n , s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let slice_op (s: string) : string = String.slice 1n 2n s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let slice_op = (s: string): string => String.slice(1n, 2n, s);
```

</Syntax>


## String.sub(pos1: nat, pos2: nat, s: string) : string

Alias for `String.slice`.

## String.concat(s1: string, s2: string) : string

Concatenate two strings and return the result.



<Syntax syntax="pascaligo">

```pascaligo
function concat_op (const s : string) : string is s ^ "toto"
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let concat_syntax (s: string) = s ^ "test_literal"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let concat_syntax = (s: string) => s ++ "test_literal";
```

</Syntax>

