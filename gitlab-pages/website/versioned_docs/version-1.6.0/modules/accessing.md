---
id: accessing
title: Accessing
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

We can access the components of a module by means of the selection
operator "`.`", as with records.

Let us suppose that we keep an amount in euros using the previously
defined module `Euro`. Then, we can write a `tip` function outside
`Euro` that increments a given amount each time it is called.

```cameligo group=Euro
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end

type storage = Euro.t

let tip (s : storage) : storage =
  Euro.add (s, Euro.one)
```

In principle, we could change the implementation of `Euro`, without
having to change the `storage` type or the function `tip`. For
example, if we decide later that we should support manipulating
negative values, we could change `Euro` as follows:

```cameligo group=module_accessing
module Euro =
  struct
    type t = int
    let add (a, b : t * t) : t = a + b
    let one : t = 1
    let two : t = 2
  end
```

The code of `tip` still works, and no change is needed. Abstraction
accomplished!

Note that code using the module `Euro` might still break the
abstraction if it directly uses the underlying representation of
`Euro.t`. Client code should always try to respect the interface
provided by the module, and not make assumptions on its current
underlying representation. For example, `Euro.t` is a transparent
alias of `nat` (or `int`). In order to hide the representation of a
type in a module, we need to constrain the module with a module type,
or [*signature*](../signatures/declaring.md).

</Syntax>


<Syntax syntax="jsligo">

We can access the components of a namespace by means of the selection
operator "`.`", as with records.

Let us suppose that we keep an amount in euros using the previously
defined namespace `Euro`. Then, we can write a `tip` function outside
`Euro` that increments a given amount each time it is called.

```jsligo group=Euro
namespace Euro {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};

type storage = Euro.t;

const tip = (s : storage) : storage =>
  Euro.add (s, Euro.one);
```

In principle, we could change the implementation of `Euro`, without
having to change the `storage` type or the function `tip`. For
example, if we decide later that we should support manipulating
negative values, we could change `Euro` as follows:

```jsligo group=module_accessing
namespace Euro {
  export type t = int;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1;
  export const two: t = 2;
};
```

The code of `tip` still works, and no change is needed. Abstraction
accomplished!

Note that code using the namespace `Euro` might still break the
abstraction if it directly uses the underlying representation of
`Euro.t`. Client code should always try to respect the interface
provided by the namespace, and not make assumptions on its current
underlying representation. For example, `Euro.t` is a transparent
alias of `nat` (or `int`). In order to hide the representation of a
type in a namespace, we need to constrain the namesapce with an
[*interface*](../signatures/declaring.md).

</Syntax>
