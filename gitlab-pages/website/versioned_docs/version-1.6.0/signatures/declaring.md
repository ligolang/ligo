---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Until now, we dealt with implicit module types, also know as
*signatures*. Having explicitly declared module types enables
abstraction and reusability by inclusion of signatures. Signatures are
defined like in OCaml, that is, they are introduced by the keywords
`module type` and the signature expression is in between `sig` and
`end`:

```cameligo group=sig_declaring
module type Euro_SIG =
  sig
    type t
    val add : t * t -> t
    val one : t
    val two : t
  end
```

The rationale for using module types is the same as using explicit
type annotation for values: to make sure our values comply with their
intended use. (We will see other reasons in the next sections.) When
the declaration of a module is explicitly constrained by a signature,
its contents must match *exactly* that of the signature (that is,
nothing less, nothing more), also known as *filtering semantics*:

```cameligo group=sig_declaring
module Euro : Euro_SIG =
  struct
    type t = nat // No more abstract
    let add (a, b : t * t) = a + b
    let one : t = 1n
    let two : t = 2n
  end
```

Note how module definitions *must* instantiate any abstract type in
their signature: here `Euro.t`.

</Syntax>

<Syntax syntax="jsligo">

Until now, we dealt with implicit namespace types, also know as
*interfaces*. Having explicitly declared interfaces enables
abstraction and reusability by inclusion of interfaces. Interfaces are
introduced by the keyword `interface`:

```jsligo group=intf_declaring
interface Euro_INTF {
  type t;
  const add: (a: t, b: t) => t;
  const one: t;
  const two: t;
};
```

An interface can then be used to constrain a namespace definition,
ensuring that said namespace contains *at least* the types and values
listed in the given interface. An interface constraint is introduced
by the keyword `implements`:

```jsligo group=intf_declaring
namespace Euro implements Euro_INTF {
  export type t = nat; // No more abstract
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};
```

Note how namespace definitions *must* instantiate any abstract type in
their interface: here `Euro.t`.

</Syntax>
