---
id: strings
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
