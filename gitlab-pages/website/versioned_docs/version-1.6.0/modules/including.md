---
id: including
title: Including
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

When writing a new version of a module, it is often needed to add new
features, that is, new types and values, for example when implementing
the next version of a standard. This can be achieved by defining a new
module that includes the types and values of the old one, and defines
new ones.

In the following example, let us extend the `Euro` module with a 10
euro note. The inclusion of a module `M` is specified with a field
`include M`, like so:

```cameligo group=including
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

module NewEuro =
  struct
    include Euro
    let ten : t = 10n
  end
```

</Syntax>

<Syntax syntax="jsligo">
This feature is not available in JsLIGO.
</Syntax>
