---
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

## Unit

The type `unit` is a predefined type that contains only one value that
carries no information. It is used when no relevant information is
required or produced.

<Syntax syntax="cameligo">

The unique value of type `unit` is written `()`, like an empty tuple,
following the OCaml convention.

```cameligo group=unit
let x : unit = ()
```

Imperative statements, like statements and loops, will have type
`unit`, and that is why it is documented here.

</Syntax>

<Syntax syntax="jsligo">

The unique value of type `unit` is `[]`, like an empty tuple.

```jsligo group=unit
const x : unit = [];
```

</Syntax>

## Options

<Syntax syntax="cameligo">

The `option` type is a parametric, predefined variant type that is
used to express whether there is a value of some type or none. This is
especially useful when calling a *partial function*, that is, a
function that is not defined for some inputs. In that case, the value
of the `option` type would be `None`, otherwise `Some (v)`, where `v`
is some meaningful value *of any type*. A typical example from
arithmetics is the division:

```cameligo group=options
let div (a, b : nat * nat) : nat option =
  if b = 0n then None else Some (a/b)
```

Note: See the predefined
[module Option](../reference/option-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

The `option` type is a parametric, predefined variant type that is
used to express whether there is a value of some type or none. This is
especially useful when calling a *partial function*, that is, a
function that is not defined for some inputs. In that case, the value
of the `option` type would be `None()`, otherwise `Some(v)`, where `v`
is some meaningful value *of any type*. A typical example from
arithmetics is the division:

```jsligo group=options
function div (a: nat, b: nat): option<nat> {
  if (b == 0n) return None() else return Some(a/b)
};
```

Note: See the predefined
[namespace Option](../reference/option-reference/?lang=jsligo)

</Syntax>

## Matching

Variant types being, in essence, the disjunctive union of cases akin
to types, values of such types need to be examined case by case: this
is what *pattern matching* does.

Here is a function that transforms a colour variant type to an integer.

<Syntax syntax="cameligo">

```cameligo group=variant_matching
type colour =
  | RGB of int * int * int
  | Gray of int
  | Default

let int_of_colour (c : colour) : int =
  match c with
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i
  | Default -> 0
```

> Note: This is the same construct as in OCaml.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=variant_matching
type colour =
| ["RGB", [int, int, int]]
| ["Gray", int]
| ["Default"];

const int_of_colour = (c : colour) : int =>
  match(c) {
    when(RGB([r,g,b])): 16 + b + g * 6 + r * 36;
    when(Gray(i)): 232 + i;
    when(Default): 0;
  };
```

> Note: The `when`-clauses must cover all the variants of the type
> `colour`. When the constructor has no argument, which is equivalent
> to having a `[]` (unit) argument, it can be omitted, hence
> `when(Default)` instead of `when(Default())`.

The right-hand sides of each `when`-clause is an expression. Sometimes
we might need statements to be processed before a value is given to
the clause. In that case, the `do` expression comes handy. It enables
the opening of a block of statements like a function body, that is, a
block ended with a `return` statement whose argument has the value of
the block, like so:

```jsligo group=match_with_block
function match_with_block (x : option<int>) : int {
  return
    match(x) {
      when(None): 0;
      when(Some(n)): do {
        let y = n + 1;
        return y
      }
    };
};
```

</Syntax>

Another example is matching on whether an integer is a natural number
or not:

<Syntax syntax="cameligo">

```cameligo group=nat_matching
let is_it_a_nat (i : int) =
  match is_nat i with
    None   -> false
  | Some _ -> true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=nat_matching
const is_it_a_nat = (i : int) =>
  match (is_nat(i)) {
    when(None): false;
    when(Some(n)): do {ignore(n); return true; }
  }
```

</Syntax>
