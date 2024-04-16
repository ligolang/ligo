---
id: parametric_types
title: Parameteric types
---

import Syntax from '@theme/Syntax';

When values are parameterised by values, we obtain
functions. Similarly, when a type is parameterised by a type, we
obtain *parametric types*.

For example, we might want to define a type for a pair made of a
string and an open type parameter, which be call a binding of a key
and a value:

<Syntax syntax="cameligo">

```cameligo group=parametric_types
type key = string
type 'value binding = key * 'value

let int_binding : int binding = "Alice", 4
let string_binding : string binding = "Bob", "cat"
```

Note how, like in OCaml, the type parameter `'value` requires to be
prefixed by a quote (tick) to be distinguished from an already defined
type. Also, the type parameter is placed before the type name, as in
`int binding`.

</Syntax>


<Syntax syntax="jsligo">

```jsligo group=parametric_types
type key = string;
type binding<value> = [key, value];

const int_binding : binding<int> = ["Alice", 4];
const string_binding : binding<string> = ["Bob", "cat"];
```

</Syntax>
