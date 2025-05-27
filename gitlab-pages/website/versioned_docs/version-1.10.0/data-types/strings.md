---
title: Strings
---

import Syntax from '@theme/Syntax';

Strings are of the predefined type `string`. Literal strings are set
between double quotes.

<Syntax syntax="cameligo">

```cameligo group=strings
let a : string = "Hello Alice"
```

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=strings
const a :string = "Hello Alice";
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>

### Casting

Strings can be used in contexts where a boolean is expected: an empty
string is then interpreted as `false`, and `true` otherwise.

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

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

Strings can be concatenated using the overloaded `+` operator, like
so:

```jsligo group=concatenating
const name = "Alice";
const greeting = "Hello";
const full_greeting = greeting + " " + name;
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>

## Sizing

The length of a string can be obtain by calling the predefined
functions `String.length` or `String.size`:

<Syntax syntax="cameligo">

```cameligo group=length
let length : nat = String.size "Alice" // length = 5n
```

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=length
const length : nat = String.size("Alice"); // length == 5n
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>

## Slicing

Substrings can be extracted using the predefined function
`String.sub`. The first character has index 0 and the interval of
indices for the substring has inclusive bounds.

<Syntax syntax="cameligo">

```cameligo group=slicing
let name  = "Alice"
let slice = String.sub 0n 1n name  // slice = "A"
```

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

The offset and length of the slice are natural number:

```jsligo group=slicing
const name = "Alice";
const slice = String.sub (0n, 1n, name); // slice == "A"
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>

## Verbatim

Strings can contain control characters, like `\n`. Sometimes we need
that each character in a string is interpreted on its own, for example
`\n` as two characters instead of a newline character. In that case,
either we escape the backslash character, or we use <em>verbatim
strings</em>. Those have the same type `string` as normal (that is,
interpreted) strings.

<Syntax syntax="cameligo">

Verbatim strings are given between the delimiters `{|` and `|}`,
instead of double quotes:

```cameligo group=verbatim
let s : string = {|\n|} // String made of two characters
```

Note: See the predefined
[module String](../reference/string-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

Verbatim strings are given between backquotes (a.k.a. backticks),
instead of double quotes:

```jsligo group=verbatim
const s : string = `\n` // String made of two characters
```

Note: See predefined [namespace String](../reference/string-reference/?lang=jsligo)

</Syntax>

