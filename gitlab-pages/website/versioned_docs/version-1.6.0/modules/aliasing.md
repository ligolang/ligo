---
id: aliasing
title: Aliasing
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

CameLIGO supports module aliases, that is, modules that work as
synonyms of other (previously defined) modules. This feature can be
useful if we implement a module using a previously defined one, but in
the future, we might need to change it. For example, until 2025, the
Bulgarian Lev is pegged to the euro currency:

```cameligo group=Lev
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

module Bulgarian_Lev = Euro
```

</Syntax>

<Syntax syntax="jsligo">

JsLIGO supports namespace aliases, that is, namespaces that work as
synonyms of other (previously defined) namespaces. This feature can be
useful if we implement a namespace using a previously defined one, but
in the future, we might need to change it. For example, until 2025,
the Bulgarian Lev is pegged to the euro currency:

```jsligo group=Lev
namespace Euro {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};

import Bulgarian_Lev = Euro;
```

Note the keyword `import`, even if the module `Euro` is defined in the
same file (this might be a bit counter-intuitive, but this is the
convention).

</Syntax>
