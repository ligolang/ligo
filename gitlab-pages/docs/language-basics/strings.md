---
id: strings
title: Strings
---

Strings are defined using the built-in `string` type like this:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```
const a : string = "Hello Alice"
```
<!--CameLIGO-->
```
let a : string = "Hello Alice"
```
<!--ReasonLIGO-->
```reasonligo
let a : string = "Hello Alice";
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Concatenating Strings

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
Strings can be concatenated using the `^` operator.

```pascaligo group=a
const name : string = "Alice"
const greeting : string = "Hello"
const full_greeting : string = greeting ^ " " ^ name
```
<!--CameLIGO-->
Strings can be concatenated using the `^` operator.

```cameligo group=a
let name : string = "Alice"
let greeting : string = "Hello"
let full_greeting : string = greeting ^ " " ^ name
```
<!--ReasonLIGO-->
Strings can be concatenated using the `++` operator.

```reasonligo group=a
let name : string = "Alice";
let greeting : string = "Hello";
let full_greeting : string = greeting ++ " " ++ name;
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Slicing Strings

Strings can be sliced using a built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=b
const name  : string = "Alice"
const slice : string = string_slice (0n, 1n, name)
```
<!--CameLIGO-->
```cameligo group=b
let name  : string = "Alice"
let slice : string = String.slice 0n 1n name
```
<!--ReasonLIGO-->
```reasonligo group=b
let name  : string = "Alice";
let slice : string = String.slice (0n, 1n, name);
```
<!--END_DOCUSAURUS_CODE_TABS-->

> ⚠️ Notice that the offset and length of the slice are natural numbers.

## Length of Strings

The length of a string can be found using a built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
const name : string = "Alice"
const length : nat = size (name)   // length = 5
```
<!--CameLIGO-->
```cameligo group=c
let name : string = "Alice"
let length : nat = String.size name  // length = 5
```

<!--ReasonLIGO-->
```reasonligo group=c
let name : string = "Alice";
let length : nat = String.size (name);  // length == 5
```
<!--END_DOCUSAURUS_CODE_TABS-->
