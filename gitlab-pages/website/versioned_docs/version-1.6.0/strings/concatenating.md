---
id: concatenating
title: Concatenating
---

import Syntax from '@theme/Syntax';

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
