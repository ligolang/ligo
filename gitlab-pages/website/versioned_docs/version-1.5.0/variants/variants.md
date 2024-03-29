---
id: variants
title: Variants
---

import Syntax from '@theme/Syntax';

A variant type is a type that defines a type by the union of
non-overlapping cases, so a value of a variant type is either this, or
that or... The simplest variant type is equivalent to the enumerated
types found in Java, C++, JavaScript etc.

Here is how we define a coin as being either head or tail (and nothing
else):

<Syntax syntax="cameligo">

```cameligo group=variants
type coin = Head | Tail
let head : coin = Head
let tail : coin = Tail
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=variants
type coin = ["Head"] | ["Tail"];
let head: coin = Head();
let tail: coin = Tail();
```

</Syntax>

The names `Head` and `Tail` in the definition of the type `coin` are
called *data constructors*, or *variants*. In this particular case,
they carry no information beyond their names, so they are called
*constant constructors*.

In general, it is interesting for variants to carry some information,
and thus go beyond enumerated types. In the following, we show how to
define different kinds of users of a system.

<Syntax syntax="cameligo">

```cameligo group=variants
type id = nat

type user =
  Admin   of id
| Manager of id
| Guest

let bob : user = Admin 1000n
let carl : user = Guest
```

A constant constructor is equivalent to the same constructor taking an
argument of type `unit`, so, for example, `Guest` is the same value as
`Guest ()`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=variants
type id = nat;

type user =
  ["Admin", id]
| ["Manager", id]
| ["Guest"];

const bob : user = Admin(1000n);
const carl : user = Guest();
```

A constant constructor is equivalent to the same constructor taking an
argument of type `unit`, so, for example, `Guest()` is the same value
as `Guest([])` or `Guest(unit)`.

</Syntax>
