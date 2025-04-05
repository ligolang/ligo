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

An interface is a list properties that you can apply to a class.  When
you apply an interface to a class, that class must have all of the
properties declared in the interface.  The class can also have
additional definitions that are not in the interface.  The LIGO
compiler warns you of any mismatches between the interface and the
class.

For example, the following interface specifies that a class must have these contents:

- A property named `add`, which accepts two values of type `nat` and returns a value of type `nat`
- Values `one` and `two`, which are of the type `nat`

```jsligo group=intf_declaring
interface Euro_INTF {
  add: (a: nat, b: nat) => nat;
  one: nat;
  two: nat;
};
```

To apply an interface to a class, put the name of the interface after
the keyword `implements` and the class name, as in the following
example.  We say that the class *implements* the interface.  This
class defines the properties `add`, `one` and `two`.  It also adds a
property named `multiply` that is not specified in the interface:

```jsligo group=intf_declaring
class Euro implements Euro_INTF {
  static add = (a: nat, b: nat): nat => a + b;
  static one = 1 as nat;
  static two : nat = 2 as nat;
  multiply = (a: nat, b: nat): nat => a * b;
};
```

Note how properties from the interface must be defined as `static` in
the class.

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
  add: (a: nat, b: nat) => nat;
  one: nat;
  two: nat;
};

interface WithTenEuro_INTF extends Euro_INTF {
  ten: nat;
};

interface WithFiftyEuro_INTF extends Euro_INTF {
  fifty: nat;
};
```

Interfaces can extend more than one interface, which can lead to an
interface that extends a base interface more than once, known as
*diamond inheritance*.  For example, the following interface extends
two interfaces from the previous example.  Because both of these
interfaces extend the same base interface, it is as if the interface
extends the base interface twice.  Diamond inheritance doesn't cause
any problems for the interface.

```jsligo group=intf_extending
interface NewEuro_INTF
  extends WithTenEuro_INTF, WithFiftyEuro_INTF {
  hundred: nat;
  five_hundred?: nat;
};
```

Interfaces can have optional types and values indicated with a
question mark `?`.  In the previous example, the interface
`NewEuro_INTF` has an optional property `five_hundred`.  This class
defines this optional value and adds a property named `twenty` that is
not defined in the `NewEuro_INTF` interface:

```jsligo group=intf_extending
class NewEuro implements NewEuro_INTF {
  static add = (a: nat, b: nat) => a + b;

  static one: nat = 1;
  static two: nat = 2;
  static ten: nat = 10;
  static fifty: nat = 50;
  static hundred: nat = 100;
  static five_hundred: nat = 500; // Could be omitted
  static twenty: nat = 20; // Extra new constant
}
```

</Syntax>
