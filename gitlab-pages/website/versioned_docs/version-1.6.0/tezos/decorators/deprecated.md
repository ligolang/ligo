---
id: deprecated
title: deprecated
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@deprecated "Some explanation."]` is used in libraries
to deprecate some values. For instance

```cameligo group=deprecated
[@inline] [@deprecated "Use `List.tail` instead."]
let tail_opt (type elt) (list: elt List.t) : elt List.t option =
  List.tail list
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@deprecated("Some explanation.")` is used in libraries
to deprecate some values. For instance, in the module `List` of the
standard library:

```jsligo group=deprecated
@inline @deprecated("Use `List.tail` instead.")
const tail_opt = <elt>(list: List.t<elt>) : option<List.t<elt>> =>
  List.tail(list);
```

</Syntax>
