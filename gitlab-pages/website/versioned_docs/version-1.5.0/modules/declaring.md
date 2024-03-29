---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Modules are a programming language construction that allows us to
package related definitions together. A canonical example of a module
is a data type and associated operations over it (e.g. stacks or
queues). The rest of the program can access these definitions in a
regular way, providing maintainability, reusability and safety.

By contrast, a record cannot package type definitions together with
values: modules can, but records are values and modules are not values:
this is where design comes into play: do we want to create a kind of
library, which we use for other tasks, or do we want to compute? If
the former, we would probably use a module; if the latter, a record.

For a concrete example, we could create a module that packages a type
that represents amounts in a particular currency together with
functions that manipulate these amounts: constants, addition,
subtraction, etc.

Modules are introduced by the keyword `module`, and type and value
definitions are grouped within a block opened by the keyword `struct`,
and closed by the keyword `end`, called a *structure*, as in
OCaml. Module names must start with a capital letter.

For example, the following code defines a module `Euro` that packages
together a type, called `t`, together with an operation `add` that
sums two values of the given currency, as well as coins for one and
two euros.

```cameligo group=Euro
module Euro =
  struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    let one : t = 1n
    let two : t = 2n
  end
```

By default all the definitions in a module are "exported", that is,
they are accessible from outside the module. (We will see how to
restrict the access by means of
[module types](../signatures/declaring.md).)

</Syntax>

<Syntax syntax="jsligo">

Namespaces are a programming language construction that allows us to
package related definitions together. A canonical example of a
namespace is a data type and associated operations over it
(e.g. stacks or queues). The rest of the program can access these
definitions in a regular way, providing maintainability, reusability
and safety.

By contrast, a record cannot package type definitions together with
values: namespaces can, but records are values and namespaces are not
values: this is where design comes into play: do we want to create a
kind of library, which we use for other tasks, or do we want to
compute? If the former, we would probably use a namespace; if the
latter, an record.

For a concrete example, we could create a namespace that packages a
type that represents amounts in a particular currency together with
functions that manipulate these amounts: constants, addition,
subtraction, etc.

Namespaces are introduced using the keyword `namespace`, and type and
value definitions are grouped within a block opened by "`{`" and
closed by "`}`". Namespace names must start with a capital letter.

For example, the following code defines a module `Euro` that packages
together a type, called `t`, together with an operation `add` that
sums two values of the given currency, as well as coins for one and
two euros.

```jsligo group=Euro
namespace Euro {
  export type t = nat;
  export const add = (a: t, b: t) : t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
};
```

In this example you will also notice how all definitions are prefixed
by the keyword `export`: this enables access to them from outside the
namespace.

</Syntax>
