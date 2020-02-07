---
id: string-reference
title: String
---

## String.size(s: string) : nat

Get the size of a string. [Michelson only supports ASCII strings](http://tezos.gitlab.io/whitedoc/michelson.html#constants) 
so for now you can assume that each character takes one byte of storage.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function string_size (const s: string) : nat is size(s)
```

<!--CameLIGO-->
```cameligo
let size_op (s: string) : nat = String.size s
```

<!--ReasonLIGO-->
```reasonligo
let size_op = (s: string): nat => String.size(s);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## String.length(s: string) : nat

Alias for `String.size`.

## String.slice(pos1: nat, pos2: nat, s: string) : string

Get the substring of `s` between `pos1` inclusive and `pos2` inclusive. For example
the string "tata" given to the function below would return "at".

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo
function slice_op (const s : string) : string is
  begin skip end with string_slice(1n , 2n , s)
```
<!--CameLIGO-->
```cameligo
let slice_op (s: string) : string = String.slice 1n 2n s
```
<!--ReasonLIGO-->
```reasonligo
let slice_op = (s: string): string => String.slice(1n, 2n, s);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## String.sub(pos1: nat, pos2: nat, s: string) : string

Alias for `String.slice`.

## String.concat(s1: string, s2: string) : string

Concatenate two strings and return the result.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function concat_op (const s : string) : string is
  begin skip end with string_concat(s , "toto")
```

<!--CameLIGO-->
```cameligo
let concat_syntax (s: string) = s ^ "test_literal"
```

<!--ReasonLIGO-->
```reasonligo
let concat_syntax = (s: string) => s ++ "test_literal";
```

<!--END_DOCUSAURUS_CODE_TABS-->
