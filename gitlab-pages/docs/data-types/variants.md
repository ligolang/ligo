---
title: Variants
---

import Syntax from '@theme/Syntax';

Variant types have one or more non-overlapping cases.
A value of a variant type can be one of these cases or another but never more than one case.

Simple variant types are similar to enumerated types found in many other languages.
For example, this type defines a coin as being either heads or tails (and nothing else):

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
let head: coin = ["Head" as "Head"];
let tail: coin = ["Tail" as "Tail"];
```

</Syntax>

The names `Head` and `Tail` in the definition of the type `coin` are called *data constructors*.
In this particular case, they carry no information beyond their names, so they are called *constant constructors*.

Variants can carry more information than just their constructors.
Each constructor in a variant can specify types that the constructor contains.
For example, this variant types defines different kinds of users of a system, with some having an ID number and others not:

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

const bob : user = ["Admin" as "Admin", 1000 as nat];
const carl : user = ["Guest" as "Guest"];
```

A constant constructor is equivalent to the same constructor taking a value of type `unit`, so, for example, `["Guest" as "Guest"]` is the same value as `["Guest" as "Guest", []]` and `["Guest" as "Guest", unit]`.

:::note

To create a variable of a variant type, you must specify its value as a tuple where the first value is the constructor and the second value is the value that the constructor takes.
To prevent the compiler from seeing the name of the constructor as a string, you must set its type as one of the constructors from the variant type with the `as` syntax, as in the previous examples.

:::

</Syntax>

## Unit

The type `unit` is a predefined type that contains only one value that
carries no information.
It is used when no relevant information is
required or produced, as in constant constructors.

<Syntax syntax="cameligo">

The unique value of type `unit` is written `()`, like an empty tuple,
following the OCaml convention.

```cameligo group=unit
let x: unit = ()
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

The option type is a predefined variant type that has two cases: `Some(v)`, where `v` is some value of any type, and `None`.

Some functions return options when they are not defined for certain inputs.
For example, you can get a value from a big map by passing the key to the `Big_map.find_opt` function.
This function returns an option that is `Some` with the value if the key is defined in the big map or `None` with unit if it is not.

Similarly, division by zero is not defined, so this function divides two numbers and returns `Some` if the result is defined or `None` if it is not:

<Syntax syntax="cameligo">

```cameligo group=options
let div (a, b : nat * nat) : nat option =
  if b = 0n then None else Some (a/b)
```

Note: See the predefined
[module Option](../reference/option-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=options
function div (a: nat, b: nat): option<nat> {
  if (b == (0 as nat)) return ["None" as "None"];
  return ["Some" as "Some", a/b]
};
```

Note: See the predefined
[namespace Option](../reference/option-reference/?lang=jsligo)

</Syntax>

## Matching

To work with variants and options, you must handle each case of the type.
LIGO handles different cases by *pattern matching*, which uses the `$match` predefined function to run different code for each case.
The `$match` function must cover all of the cases of the type.

For example, the following code defines a colour variant type and a function that converts values of that type to a single integer by using pattern matching on each variant:

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
  $match(c, {
    "RGB": ([r,g,b]) => 16 + b + g * 6 + r * 36,
    "Gray": i => 232 + i,
    "Default": () => 0
  });
```

As its parameters, the `$match` function receives the variant value to match on and an object.
The property names of the object are the constructors of the corresponding variant type.
The property values are either a single expression or expressions that receive the value of the variant as a parameter.
When the variant has no value, as in constant constructors or the `None` variant, the expression receives unit as a parameter.

For complex expressions, you can use an immediately invoked function expression (IIFE) as the result of a match.
This allows you to use a block of statements with a `return` statement like a function body, as in this example:

```jsligo group=match_with_block
const match_with_block = (x : option<int>) : int =>
  $match(x, {
    "None": () => 0,
    "Some": n => (() => {
      const y = n + 1;
      return y;
    })(),
  });
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
  $match(is_nat(i), {
    "None": () => false,
    "Some": n => (() => { ignore(n); return true; })()
  })
```

</Syntax>
