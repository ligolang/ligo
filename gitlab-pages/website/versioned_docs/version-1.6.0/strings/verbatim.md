---
id: verbatim
title: Verbatim
---

import Syntax from '@theme/Syntax';

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
