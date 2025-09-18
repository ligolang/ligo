---
id: deprecated
title: deprecated
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@deprecated "Some explanation."]` is used in libraries to deprecate values and often to provide information about what values to use instead.

For example, the `List` module of the standard library has a deprecated function named `tail_opt`.
Its `@deprecated` attribute shows what function to use instead:

```cameligo group=deprecated
[@inline] [@deprecated "Use `List.tail` instead."]
let tail_opt (type elt) (list: elt List.t) : elt List.t option =
  List.tail list
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@deprecated("Some explanation.")` is used in libraries
to deprecate values and often to provide information about what values to use instead.

For example, the `List` namespace of the standard library has a deprecated function named `tail_opt`.
Its `@deprecated` attribute shows what function to use instead:

```jsligo group=deprecated
@inline @deprecated("Use `List.tail` instead.")
const tail_opt = <elt>(list: List.t<elt>) : option<List.t<elt>> =>
  List.tail(list);
```

</Syntax>
