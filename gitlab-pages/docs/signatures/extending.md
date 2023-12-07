---
id: extending
title: Extending
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Like modules, signatures can be extended by including another.

```cameligo group=sig_including
module type Euro_SIG =
  sig
    type t
    val add : t * t -> t
    val one : t
    val two : t
  end

module type NewEuro_SIG =
  sig
    include Euro_SIG
    val ten : t
  end
```

Note how the type `t` remains abstract in `NewEuro_SIG`.

</Syntax>

<Syntax syntax="jsligo">

Unlike namespaces, interfaces can be *extended by inheritance*. The
construct is introduced by the keyword `extends`, like so

```jsligo group=intf_extending
interface Euro_INTF {
  type t;
  const add: (a: t, b: t) => t;
  const one: t;
  const two: t;
};

interface WithTenEuro_INTF extends Euro_INTF {
  const ten: t;
};

interface WithFiftyEuro_INTF extends Euro_INTF {
  const fifty: t;
};
```

Note how the type `t` remains abstract in `WithTenEuro_INTF` and `WithFiftyEuro_INTF`.

It is possible to design *diamond inheritance*, that is, inheriting
twice the same base interface, like so:

```jsligo group=intf_extending
interface NewEuro_INTF
  extends WithTenEuro_INTF, WithFiftyEuro_INTF {
  const hundred: t;
  const five_hundred?: t;
};
```

Here, the abstract type `t` was inherited twice from `Euro_INTF`. Note
the *optional value* `five_hundred`, distinghished as such by a
question mark: `five_hundred?`. This means that a namespace
implementing `NewEuro_INTF` can choose not to implement `five_hundred`
(because it is often counterfeited). The implementation of an
interface can be done as follows:

```jsligo group=intf_extending
namespace NewEuro implements NewEuro_INTF {
  type t = int;

  const add = (a: t, b: t) => a + b;

  const one: t = 1;
  const two: t = 2;
  const ten: t = 10;
  const fifty: t = 50;
  const hundred: t = 100;
  const five_hundred: t = 500; // Could be omitted
  const twenty: t = 20; // Extra new constant
}
```

Note how `five_hundred` was indeed implemented, although it was not
mandatory, and how we added `twenty`, even it is not found in any of
the inherited signatures.

</Syntax>
