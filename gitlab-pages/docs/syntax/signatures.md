---
id: signatures
title: Signatures
jsligoTitle: Interfaces
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

A signature is a list of types and values that you can apply to a module.
When you apply a signature to a module, that module must have all of the types and values in the signature and no types or values that are not in the signature.
The LIGO compiler warns you of any mismatches between the signature and the module.

As in OCaml, to create a signature, use the keywords `module type` and list the types and values between the keywords `sig` and `end`.
For example, the following signature specifies that a module must have these contents:

- A type named `t`, although the data type of that type is not specified, which makes it an *abstract type*
- A function named `add`, which accepts two values of type `t` and returns a value of type `t`
- Values `one` and `two`, which are of the type `t`

```cameligo group=sig_declaring
module type Euro_SIG =
  sig
    type t
    val add : t * t -> t
    val one : t
    val two : t
  end
```

To apply a signature to a module, put the name of the signature after a colon and the module name, as in the following example.
This module defines the type `t` as a nat and defines the `add` function and the `one` and `two` values:

```cameligo group=sig_declaring
module Euro : Euro_SIG =
  struct
    type t = nat
    let add (a, b : t * t) = a + b
    let one : t = 1n
    let two : t = 2n
  end
```

The module must instantiate any abstract type in the signature, as this module defines the abstract type `t` as a nat.

</Syntax>

<Syntax syntax="jsligo">

An interface is a list of types and values that you can apply to a namespace.
When you apply an interface to a namespace, that namespace must have all of the types and values in the interface.
The namespace can also have additional definitions that are not in the interface.
The LIGO compiler warns you of any mismatches between the interface and the namespace.

For example, the following interface specifies that a namespace must have these contents:

- A type named `t`, although the data type of that type is not specified, which makes it an *abstract type*
- A function named `add`, which accepts two values of type `t` and returns a value of type `t`
- Values `one` and `two`, which are of the type `t`

```jsligo group=intf_declaring
interface Euro_INTF {
  type t;
  const add: (a: t, b: t) => t;
  const one: t;
  const two: t;
};
```

To apply an interface to a namespace, put the name of the interface after the keyword `implements` and the namespace name, as in the following example.
It's said that the namespace *implements* the interface.
This namespace defines the type `t` as a nat and defines the `add` function and the `one` and `two` values.
It also adds a function named `multiply` that is not specified in the interface:

```jsligo group=intf_declaring
namespace Euro implements Euro_INTF {
  export type t = nat; // No more abstract
  export const add = (a: t, b: t): t => a + b;
  export const one: t = 1n;
  export const two: t = 2n;
  export const multiply = (a: t, b: t): t=> a * b;
};
```

The namespace must instantiate any abstract type in the interface, as this namespace defines the abstract type `t` as a nat.

</Syntax>

<Syntax syntax="cameligo">

## Extending signatures

Like modules, you can extend signatures by including other signatures in them, as in this example:

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

Note that the type `t` remains abstract in both signatures.
Modules that use either signature must instantiate the type.

</Syntax>

<Syntax syntax="jsligo">

## Extending interfaces

Interfaces can be extended by inheritance with the `extends` keyword, as in this example:

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

Note that the type `t` remains abstract in all of the interfaces.
Namespaces that use any of these interfaces must instantiate the type.

Interfaces can extend more than one interface, which can lead to an interface that extends a base interface more than once, known as *diamond inheritance*.
For example, the following interface extends two interfaces from the previous example.
Because both of these interfaces extend the same base interface, it is as if the interface extends the base interface twice.
Diamond inheritance doesn't cause any problems for the interface.

```jsligo group=intf_extending
interface NewEuro_INTF
  extends WithTenEuro_INTF, WithFiftyEuro_INTF {
  const hundred: t;
  const five_hundred?: t;
};
```

Interfaces can have optional types and values indicated with a question mark `?`.
In the previous example, the interface `NewEuro_INTF` has an optional value `five_hundred`.
This namespace defines this optional value and adds a value named `twenty` that is not defined in the `NewEuro_INTF` interface:

```jsligo group=intf_extending
namespace NewEuro implements NewEuro_INTF {
  export type t = int;

  export const add = (a: t, b: t) => a + b;

  export const one: t = 1;
  export const two: t = 2;
  export const ten: t = 10;
  export const fifty: t = 50;
  export const hundred: t = 100;
  export const five_hundred: t = 500; // Could be omitted
  const twenty: t = 20; // Extra new constant
}
```

</Syntax>
