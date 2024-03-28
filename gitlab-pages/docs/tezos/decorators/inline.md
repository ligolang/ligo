---
id: inline
title: inline
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@inline]` on a function definition informs the
compiler that we that the code of said function must be inlined
wherever it is called. This enables some optimisations, possibly at
the expense of a larger compiled code. Benchmarks and profiling help
decide whether a function should be inlined or not.

Inlining also make it cheap to create aliases of functions. For
example:

```cameligo group=inline
[@inline]
let size (type elt) (list: elt List.t) : nat = List.length list
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@inline` on a function definition informs the compiler
that we that the code of said function must be inlined wherever it is
called. This allows some optimisations to be performed, possibly at
the expense of a larger compiled code. Benchmarks and profiling help
decide whether a function should be inlined or not.

Inlining also make it cheap to create aliases of functions. For
example:

```jsligo group=inline
@inline
const size = <elt>(list: List.t<elt>) : nat => List.length(list);
```

</Syntax>
