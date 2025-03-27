---
title: Strings
---

import Syntax from '@theme/Syntax';

Strings are of the predefined type `string`.
Literal strings are set between double quotes.

<Syntax syntax="cameligo">

```cameligo group=strings
let a : string = "Hello Alice"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=strings
const a :string = "Hello Alice";
```

</Syntax>

<Syntax syntax="cameligo">

For reference, see the predefined [module String](../reference/string-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

For reference, see the predefined [namespace String](../reference/string-reference/?lang=jsligo).

</Syntax>

Strings can be used in contexts where a boolean is expected: an empty
string is interpreted as `false` and a non-empty string is interpreted as `true`.

<Syntax syntax="cameligo">

```cameligo group=strings
let one  = if "" then 0 else 1
let zero = if "foo" then 0 else 1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=strings
const one  = "" ? 0 : 1;
const zero = "foo" ? 0 : 1;
```

</Syntax>

## Concatenating

<Syntax syntax="cameligo">

Strings can be concatenated using the `^` operator, as in OCaml:

```cameligo group=concatenating
let name = "Alice"
let greeting = "Hello"
let full_greeting = greeting ^ " " ^ name
```

</Syntax>

<Syntax syntax="jsligo">

Strings can be concatenated using the overloaded `+` operator, like
so:

```jsligo group=concatenating
const name = "Alice";
const greeting = "Hello";
const full_greeting = greeting + " " + name;
```

</Syntax>

## Sizing

To get the length of a string, use the function `String.length` or `String.size`:

<Syntax syntax="cameligo">

```cameligo group=length
let length : nat = String.size "Alice" // length = 5n
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=length
const length : nat = String.size("Alice"); // length == 5n
```

</Syntax>

## Slicing

You can extract a substring from a string with the `String.sub` function.
It accepts a nat for the index of the start of the substring and a nat for the number of characters.
Both numbers are inclusive.
The first character of a string has the index 0.

<Syntax syntax="cameligo">

```cameligo group=slicing
let name  = "Alice"
let slice = String.sub 0n 1n name  // slice = "A"
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=slicing
const name = "Alice";
const slice = String.sub (0n, 1n, name); // slice == "A"
```

</Syntax>

## Verbatim strings

Strings can contain control characters, like `\n`.
To interpret each character on its own (such as treating `\n` as two characters), you can either escape the backslash character or use _verbatim strings_.
Verbatim strings have the same type as ordinary strings (that is, interpreted strings).

<Syntax syntax="cameligo">

Verbatim strings are given between the delimiters `{|` and `|}` instead of double quotes:

```cameligo group=verbatim
let s : string = {|\n|} // String made of two characters
```

</Syntax>

<Syntax syntax="jsligo">

Verbatim strings are given between backquotes (a.k.a. backticks), instead of double quotes:

```jsligo group=verbatim
const s : string = `\n` // String made of two characters
```

</Syntax>
