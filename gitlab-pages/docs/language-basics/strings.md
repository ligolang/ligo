---
id: strings
title: Strings
---

Strings are defined using the built-in `string` type like this:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
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


## Concatenating strings

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
Strings can be concatenated using the `^` operator.

```pascaligo
const name : string = "Alice"
const greeting : string = "Hello"
const full_greeting : string = greeting ^ " " ^ name
```
<!--CameLIGO-->
Strings can be concatenated using the `^` operator.

```cameligo
let name : string = "Alice"
let greeting : string = "Hello"
let full_greeting : string = greeting ^ " " ^ name
```
<!--ReasonLIGO-->
Strings can be concatenated using the `++` operator.

```reasonligo
let name : string = "Alice";
let greeting : string = "Hello";
let full_greeting : string = greeting ++ " " ++ name;
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Slicing strings

Strings can be sliced using a built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const name  : string = "Alice"
const slice : string = string_slice (0n, 1n, name)
```
<!--CameLIGO-->
```cameligo
let name  : string = "Alice"
let slice : string = String.slice 0n 1n name
```
<!--ReasonLIGO-->
```reasonligo
let name  : string = "Alice";
let slice : string = String.slice (0n, 1n, name);
```
<!--END_DOCUSAURUS_CODE_TABS-->

> ⚠️ Notice that the `offset` and slice `length` are natural numbers
> (`nat`).

## Aquiring the length of a string

The length of a string can be found using a built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const name : string = "Alice"
// length = 5
const length : nat = size (name)
```
<!--CameLIGO-->
```cameligo
let name : string = "Alice"
let length : nat = String.size name
```

<!--ReasonLIGO-->
```reasonligo
let name : string = "Alice";
let length : nat = String.size (name);
```
<!--END_DOCUSAURUS_CODE_TABS-->
