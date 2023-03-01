---
id: unit-option-pattern-matching
title: Unit, Option, Pattern matching
---

import Syntax from '@theme/Syntax';

Optional values are a pervasive programming pattern in OCaml. Since
Michelson and LIGO are both inspired by OCaml, *optional types* are
available in LIGO as well. Similarly, OCaml features a *unit* type,
and LIGO features it as well. Both the option type and the unit types
are instances of a more general kind of types: *variant types*
(sometimes called *sum types*).

## The unit Type

The `unit` type in Michelson or LIGO is a predefined type that
contains only one value that carries no information. It is used when
no relevant information is required or produced. Here is how it used.

<Syntax syntax="cameligo">

In CameLIGO, the unique value of the `unit` type is `()`, following
the OCaml convention.
```cameligo group=a
let n : unit = ()
```

Sequences of expressions that return the `unit` type can be written
using `begin` and `end`, separating expressions using semi-colons. The
last expression, which represents the value returned, can have a
different type to `unit`:

```cameligo group=a
let m (x : int) =
  begin
    assert (x > 0);
    assert (x < 10);
    x
  end
```

</Syntax>

<Syntax syntax="jsligo">

In JsLIGO, the unique value of the `unit` type is `unit`.
```jsligo group=a
let n : unit = unit;
```

</Syntax>

<Syntax syntax="jsligo">

## Discriminated union type

The simplest form of pattern matching in JsLIGO is with help of a discriminated
union type, which should be familiar for developers coming from TypeScript.

```jsligo
type foo =
  { kind: "increment", amount: int}
| { kind: "decrement", amount: int}
| { kind: "reset"}
```

Here, the `kind` field is unique among the objects. If not, an error will be
generated. Also, if multiple fields are present which can be used as unique
field, only the first unique field will be used.

Creating an object from a discriminated union type requires all the fields
to be fully written. So for increment that would be:

```jsligo
let obj = { kind: "increment", amount: 3}
```

or

```jsligo
let obj2 = { kind: "reset" }
```

Pattern matching over a discriminated union type works like this:

```jsligo
let foo = (item: foo) => {
  let state = 0;
  switch(item.kind) {
    case "increment":
      state += item.amount;
      break
    case "decrement":
      state -= item.amount;
      break
    case "reset":
      state = 0;
      break
  }
}
```

Note that all cases of the discriminated union must be handled, if not an error
will be generated.

The "strict" rules on discriminated union types are because there currently is
no type system support for this.


</Syntax>

## Variant types

A variant type is a user-defined or a built-in type (in case of
options) that defines a type by cases, so a value of a variant type is
either this, or that or... The simplest variant type is equivalent to
the enumerated types found in Java, C++, JavaScript etc.

Here is how we define a coin as being either head or tail (and nothing
else):

<Syntax syntax="cameligo">

```cameligo group=b
type coin = Head | Tail
let head : coin = Head
let tail : coin = Tail
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
type coin = ["Head"] | ["Tail"];
let head: coin = Head();
let tail: coin = Tail();
```

</Syntax>


The names `Head` and `Tail` in the definition of the type `coin` are
called *data constructors*, or *variants*. In this particular, they
carry no information beyond their names, so they are called *constant
constructors*.

In general, it is interesting for variants to carry some information,
and thus go beyond enumerated types. In the following, we show how to
define different kinds of users of a system.

<Syntax syntax="cameligo">

```cameligo group=c
type id = nat

type user =
  Admin   of id
| Manager of id
| Guest

let u : user = Admin 1000n
let g : user = Guest
```

In CameLIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest` is the
same value as `Guest ()`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=c
type id = nat;

type user =
  ["Admin", id]
| ["Manager", id]
| ["Guest"];

let u : user = Admin(1000 as nat);
let g : user = Guest();
```

In JsLIGO, a constant constructor is equivalent to the same constructor
taking an argument of type `unit`, so, for example, `Guest ()` is the
same value as `Guest (unit)`.
</Syntax>

There are cases where several sum types match a given constructor.

In the example below, types `t1` to `t6` are all possible types for `x`.

In this case, the compiler will choose one of these types as the type of the expression,
and throw a warning stating that other types are possible.

You can add a type annotation to remove this ambiguity.

**NOTE** : The compiler will choose in priority the latest matching sum type in the current scope,
if no type is defined in this scope, it will look in the latest module, if not in the second latest etc.
Below, it will choose `t1`, and if `t1` didn't match it would have chosen `t2`, otherwise `t3`, etc.

<Syntax syntax="cameligo">

```cameligo group=multi_sum
type t2 = A of int | B of int

module MyModule = struct
  type t5 = A of int | C of bool
  type t4 = A of int | D of int

  module MySubModule = struct
    type t6 = A of int | E of tez
  end
end

module MySecondModule = struct
  type t3 = A of int | F of int
end

type t1 = A of int | G of tez

// The compiler will search above for sum types with an 'A' constructor
let x = A 42
```
</Syntax>

<Syntax syntax="jsligo">

```jsligo group=multi_sum
type t2 = ["A", int] | ["B", int];

namespace MyModule {
  type t5 = ["A", int] | ["C", bool];
  type t4 = ["A", int] | ["D", int];

  namespace MySubModule {
    type t6 = ["A", int] | ["E", tez];
  }
}

namespace MySecondModule {
  type t3 = ["A", int] | ["F", int];
}

type t1 = ["A", int] | ["G", tez];

// The compiler will search above for sum types with an 'A' constructor
const x = A(42);
```
</Syntax>

<Syntax syntax="cameligo">

In CameLigo when looking for a matching sum type, the compiler will not look in shadowed modules.
The below code will throw an error because type `t1` is in a shadowed module and thus not accessible.

```cameligo group=sum_shadow
module M = struct
  type t1 = A of int | B of int
end
module M = struct
  let y = 10
end

// This will fail because A will not be found
// let x = A 42
```
</Syntax>


## Optional values

The `option` type is a predefined variant type that is used to express
whether there is a value of some type or none. This is especially
useful when calling a *partial function*, that is, a function that is
not defined for some inputs. In that case, the value of the `option`
type would be `None`, otherwise `Some (v)`, where `v` is some
meaningful value *of any type*. An example in arithmetic is the
division operation:

<Syntax syntax="cameligo">

```cameligo group=d
let div (a, b : nat * nat) : nat option =
  if b = 0n then None else Some (a/b)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=d
let div = (a: nat, b: nat): option<nat> => {
  if(b == (0 as nat)){
    return None();
  } else {
    return (Some (a/b));
  };
};
```

</Syntax>

You can extract the value of a `Some (v)` with the function `Option.unopt (Some (v))`. In case the value is `None`, this will fail with an error.

The proper way to deal with optional values is by means of pattern matching.


## Pattern matching

*Pattern matching* is similar to the `switch` construct in
JavaScript, and can be used to route the program's control flow based
on the value of a variant, record, tuple, or list.

A component of a pattern can be discarded by using a wildcard `_`
instead of a variable name.

LIGO will warn about unused variables bound in patterns in the same
way that function arguments are warned about. Variable names beginning
with `_` can be used as a binder to prevent warnings.



<Syntax syntax="jsligo">

> Note: JsLIGO only supports basic pattern matching at the moment. This will change in the future.

</Syntax>

### Match on variants

Here is a function that transforms a colour variant type to an int.

<Syntax syntax="cameligo">

```cameligo group=pm_variant
type color =
  | RGB   of int * int * int
  | Gray  of int
  | Default

let int_of_color (c : color) : int =
  match c with
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i
  | Default -> 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=pm_variant
type color =
| ["RGB", [int, int, int]]
| ["Gray", int]
| ["Default"];

let int_of_color = (c : color) : int =>
  match(c, {
    RGB: rgb => 16 + rgb[2] + rgb[1] * 6 + rgb[0] * 36,
    Gray: i => 232 + i,
    Default: () => 0 });
```

</Syntax>

### Matching records or tuples

Fields of records and components of tuples can be destructured. Record pattern variables can be renamed.


<Syntax syntax="cameligo">

```cameligo group=pm_rec_tuple
type my_record = { a : int ; b : nat ; c : string }
type my_tuple = int * nat * string

let on_record (v : my_record) : int =
  match v with
  | { a ; b = b_renamed ; c = _ } -> a + int(b_renamed)

let on_tuple (v : my_tuple) : int =
  match v with
  | ( x , y , _ ) -> x + int(y)
```

</Syntax>

<Syntax syntax="jsligo">

Pattern-matching on records and tuples are not supported in JsLIGO yet.

</Syntax>

### Match on lists

<Syntax syntax="cameligo">

```cameligo group=pm_lists
let weird_length (v : int list) : int =
  match v with
  | [] -> -1
  | [ a; b ; c] -> -2
  | x -> int (List.length x)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=pm_lists
let weird_length = (v : list<int>) : int =>
  match(v, list([
    ([] : list<int>) => -1,
    ([hd, ...tl] : list<int>) => 1 + int(List.length(tl))
  ]));
```

</Syntax>


### Deep patterns

Pattern matching can also be used for nested patterns.

<Syntax syntax="cameligo">

```cameligo group=pm_complex
type complex_t = { a : int list option ; b : int list }


let complex = fun (x:complex_t) (y:complex_t) ->
  match (x,y) with
  | {a=None;b=_} , { a = _ ; b = _ } -> -1
  | {a=_;b=_} , { a = Some ([]) ; b = (hd::tl) } -> hd
  | {a=_;b=_} , { a = Some (hd::tl) ; b = [] } -> hd
  | {a=Some a;b=_} , _ -> int (List.length a)
```

</Syntax>
