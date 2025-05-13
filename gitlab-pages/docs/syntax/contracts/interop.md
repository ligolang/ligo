---
id: interop
title: Interoperability
---

import Syntax from '@theme/Syntax';

LIGO can work together with other smart contract languages on Tezos, such as calling them and receiving calls from them.
However, data structures from different high-level languages might have different representations in Michelson and not how LIGO structures them by default.
When a LIGO contract calls contracts that were written in other high-level languages, it must pass parameters that are compatible with the data structures in the other contracts.

## Michelson types and annotations

Michelson types consist of `or` types and `pair` types, combined with field annotations.
Field annotations add constraints on a Michelson type.
To be compatible, two types must meet these criteria:

- They must have the same types in the same structure
- Their annotations must be compatible, which means that one of the following is true:
  - Both types have no annotations
  - Both types have annotations that are identical
  - One type has annotations and the other type has no annotations

For example, this Michelson type is a pair that contains an integer and a string, both with annotations:

```michelson
(pair (int %foo) (string %bar))
```

That type is compatible with this type because it is identical:

```michelson
(pair (int %foo) (string %bar))
```

It is also compatible with this type because it has the same types minus the annotations:

```michelson
(pair int string)
```

However, it is not compatible with the following type because the primitive types have a different structure, even though the annotations are on the same types:

```michelson
(pair (string %bar) (int %foo))
```

It is also not compatible with the following type even though the following type uses the same primitive types in the same structure.
The annotations don't match and therefore the types don't match.

```michelson
(pair (int %bar) (string %foo))
```

It is also not compatible with the following type because this type has some annotations but not all of the annotations in the other type:

```michelson
(pair (int %foo) (string))
```

In this way, to call another contract, you may need to change the output Michelson format of your types to match the type that the other contract expects.

:::info

When possible, use annotations when you define or call entrypoints.
Annotations help make it clear which entrypoint you are referring to.

:::

## Setting the Michelson layout of LIGO data structures

LIGO can format types in Michelson in two basic layouts: combs and trees.
For more information about combs and trees, see [Pairs](https://docs.tezos.com/smart-contracts/data-types/complex-data-types#pairs) on docs.tezos.com.

### Right comb layout

<Syntax syntax="cameligo">

Since version 1.0, the default Michelson data representation of LIGO data structures is a right comb that matches the order of the LIGO declarations.
You can use the attribute `[@layout comb]` to make this choice
explicit, as in this example of a variant type:

```cameligo
type animal =
[@layout comb]
| Elephant
| Dog
| Cat
```

</Syntax>

<Syntax syntax="jsligo">

Since version 1.0, the default Michelson data representation of LIGO data structures is a right comb that matches the order of the LIGO declarations.
You can use the decorator `@layout("comb")` to make this choice explicit, as in this example of a variant type:

```jsligo
type animal =
// @layout("comb")
| ["Elephant"]
| ["Dog"]
| ["Cat"];
```

</Syntax>

The resulting Michelson code looks like this:

```michelson
(or
  (unit %elephant)
  (or (unit %dog)
      (unit %cat)))
```

<Syntax syntax="cameligo">

The attribute `[@layout comb]` can also be used on record types:

```cameligo
type artist =
  [@layout comb] {
  genre : string;
  since : timestamp;
  name  : string
}
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@layout("comb")` can also be used on record types:

```jsligo
type artist =
// @layout("comb")
{
  genre : string,
  since : timestamp,
  name  : string
};
```

</Syntax>

Unlike the variant type, the compiled Michelson code of a record type in a right comb is a nested pair.
The previous example looks like this in Michelson:

```michelson
(pair (string %genre) (timestamp %since) (string %name))
```

### Left-balanced tree layout

<Syntax syntax="cameligo">

Prior to version 1.0, LIGO formatted data types into alphabetically ordered left-balanced trees by default.
You can get the equivalent in version 1.0 and later with the attribute `[@layout tree]`, as in this example:

```cameligo group=orig
type animal =
[@layout tree]
| Elephant
| Dog
| Cat
```

</Syntax>

<Syntax syntax="jsligo">

Prior to version 1.0, LIGO formatted data types into alphabetically ordered left-balanced trees by default.
You can get the equivalent in version 1.0 and later with the decorator `@layout("tree")`, as in this example:

```jsligo group=orig
type animal =
// @layout("tree")
| ["Elephant"]
| ["Dog"]
| ["Cat"];
```

</Syntax>

The resulting Michelson code looks like this:

```michelson
(or
  (or
    (unit %cat)
    (unit %dog))
  (unit %elephant))
```

Note that the variant cases are ordered alphabetically.

## Setting Michelson annotations

<Syntax syntax="cameligo">

To specify the annotation for a LIGO declaration, use the `annot` attribute.
For example, this variant type has custom annotations on each case:

```cameligo group=annot
type animal =
| [@annot memory] Elephant
| [@annot face] Dog
| [@annot fish] Cat
```

</Syntax>

<Syntax syntax="jsligo">

To specify the annotation for a LIGO declaration, use the `@annot` decorator.
For example, this variant type has custom annotations on each case:

```jsligo group=annot
type animal =
| // @annot("memory")
  ["Elephant"]
| // @annot("face")
  ["Dog"]
| // @annot("fish")
  ["Cat"]
```

</Syntax>

The resulting Michelson looks like this:

```michelson
(or
  (or
    (unit %fish)
    (unit %face))
  (unit %memory))
```

<Syntax syntax="cameligo">

The attribute `[@annot <name>]` can also be used on record field
annotations:

```cameligo group=annot
type artist = {
  [@annot style] genre: string;
  [@annot from] since: timestamp;
  [@annot performer] name: string;
}
```

If the `@layout` and `@annot` attributes are not adequate for your use case, you can format Michelson types manually as described in the next section.

</Syntax>

<Syntax syntax="jsligo">

The decorator `@annot("<name>")` can also be used on record field
annotations:

```jsligo group=annot
type artist = {
  // @annot("style")
  genre: string,
  // @annot("from")
  since: timestamp,
  // @annot("performer")
  name: string
}
```

If the `@layout` and `@annot` decorators are not adequate for your use case, you can format Michelson types manually as described in the next section.

</Syntax>

## Manually formatting Michelson types

To interoperate with existing Michelson code or to be compatible with
certain development tooling, LIGO has two special interoperation
types: `michelson_or` and `michelson_pair`. These types provide the
flexibility to model the exact Michelson output, including field
annotations.

The LIGO `michelson_or` type creates a Michelson `or` type from two LIGO types and their annotations.
Similarly, the LIGO `michelson_pair` type creates a Michelson `pair` type.
In either case, you must provide annotations for both types, but if you provide an empty string, LIGO omits the annotation in the Michelson code.

Take for example the following Michelson type that we want to
interoperate with:

```michelson
(or
  (unit %z)
  (or %other
    (unit %y)
    (pair %other
      (string %x)
      (pair %other
        (int %w)
        (nat %v)))))
```

To reproduce this type we can use the following LIGO code:

<Syntax syntax="cameligo">

```cameligo
type w_and_v = (int, "w", nat, "v") michelson_pair
type x_and = (string, "x", w_and_v, "other") michelson_pair
type y_or = (unit, "y", x_and, "other") michelson_or
type z_or = (unit, "z", y_or, "other") michelson_or
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
type w_and_v = michelson_pair<[int, "w", nat, "v"]>;
type x_and = michelson_pair<[string, "x", w_and_v, "other"]>;
type y_or = michelson_or<[unit, "y", x_and, "other"]>;
type z_or = michelson_or<[unit, "z", y_or, "other"]>;
```

</Syntax>

:::info
Alternatively, if annotations are not important you can also use plain
tuples for pairs instead. Plain tuples do not have any annotations.
:::

To use variables of type `michelson_or` you have to use `M_left` and
`M_right`. `M_left` picks the left `or` case while `M_right` picks
the right `or` case. For `michelson_pair` you need to use tuples.

<Syntax syntax="cameligo">

```cameligo
let z : z_or = M_left unit

let y_1 : y_or = M_left unit
let y   : z_or = M_right y_1

let x_pair = "foo", (2, 3n)
let x_1 : y_or = M_right x_pair
let x : z_or = M_right y_1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
const z : z_or = ["M_left" as "M_left"];

const y_1 : y_or = ["M_left" as "M_left"];
const y : z_or = ["M_right" as "M_right", y_1];

const x_pair = ["foo", [2, 3 as nat]];
const x_1 : y_or = ["M_right" as "M_right", x_pair];
const x : z_or = ["M_right" as "M_right", y_1];
```

</Syntax>

## Manual data structure conversion

If you want to get your hands dirty, it is also possible to do manual
data structure conversion. The following code provides some examples:

<Syntax syntax="cameligo">

```cameligo group=helper_functions
type z_to_v =
  Z
| Y
| X
| W
| V

type w_or_v = (unit, "w", unit, "v") michelson_or
type x_or = (unit, "x", w_or_v, "other") michelson_or
type y_or = (unit, "y", x_or, "other") michelson_or
type z_or = (unit, "z", y_or, "other") michelson_or

type test = {
  z: string;
  y: int;
  x: string;
  w: bool;
  v: int
}

let make_concrete_sum (r: z_to_v) : z_or =
  match r with
    Z -> M_left (unit)
  | Y -> M_right (M_left (unit))
  | X -> M_right (M_right (M_left (unit)))
  | W -> M_right (M_right (M_right (M_left (unit))))
  | V -> M_right (M_right (M_right (M_right (unit))))

let make_concrete_record (r: test) =
  (r.z, r.y, r.x, r.w, r.v)

let make_abstract_sum (z_or: z_or) =
  match z_or with
  | M_left n -> Z
  | M_right y_or ->
    (match y_or with
    | M_left n -> Y
    | M_right x_or -> (
        match x_or with
        | M_left n -> X
        | M_right w_or -> (
            match w_or with
            | M_left n -> W
            | M_right n -> V)))

let make_abstract_record z y x w v = {z; y; x; w; v}
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=helper_functions
type z_to_v =
  ["Z"]
| ["Y"]
| ["X"]
| ["W"]
| ["V"];

type w_or_v = michelson_or<[unit, "w", unit, "v"]>;
type x_or = michelson_or<[unit, "x", w_or_v, "other"]>;
type y_or = michelson_or<[unit, "y", x_or, "other"]>;
type z_or = michelson_or<[unit, "z", y_or, "other"]>;

type test = {
  z: string,
  y: int,
  x: string,
  w: bool,
  v: int
};

const make_concrete_sum = (r: z_to_v): z_or =>
  $match(r, {
    "Z": () => ["M_left" as "M_left"],
    "Y": () => ["M_right" as "M_right", ["M_left" as "M_left"]],
    "X": () => ["M_right" as "M_right", ["M_right" as "M_right",
                                         ["M_left" as "M_left"]]],
    "W": () => ["M_right" as "M_right",
                ["M_right" as "M_right",
                 ["M_right" as "M_right", ["M_left" as "M_left"]]]],
    "V": () => ["M_right" as "M_right", ["M_right" as "M_right",
                                         ["M_right" as "M_right",
                                          ["M_right" as "M_right"]]]]
  });

const make_concrete_record = (r: test) =>
  [r.z, r.y, r.x, r.w, r.v];

const make_abstract_sum = (z_or: z_or): z_to_v =>
  $match(z_or, {
    "M_left": _ => ["Z" as "Z"],
    "M_right": y_or =>
      $match(y_or, {
        "M_left": _ => ["Y" as "Y"],
        "M_right": x_or =>
          $match(x_or, {
            "M_left": _ => ["X" as "X"],
            "M_right": w_or =>
              $match(w_or, {
                "M_left": _ => ["W" as "W"],
                "M_right": _ => ["V" as "V"]
              })
          })
      })
  });

const make_abstract_record =
  (z: string, y: int, x: string, w: bool, v: int) => ({z,y,x,w,v});
```

</Syntax>

## Entrypoints and annotations

When a contract has multiple entrypoints, LIGO implicitly creates a parameter for it with a variant type.
For example, the following contract has entrypoints named `left` and `right`:

<Syntax syntax="cameligo">

```cameligo group=entrypoints_and_annotations
type storage = int

[@entry]
let left (i : int) (x : storage) : operation list * storage = [], x - i

[@entry]
let right (i : int) (x : storage) : operation list * storage = [], x + i
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=entrypoints_and_annotations
type storage = int

// @entry
const left = (i: int, x: storage) : [list<operation>, storage] =>
  [[], x - i]

// @entry
const right = (i: int, x: storage) : [list<operation>, storage] =>
  [[], x + i]
```

</Syntax>

Internally, LIGO structures the contract with a parameter type:

<Syntax syntax="cameligo">

```cameligo
type storage = int

type parameter =
 | Left of int
 | Right of int

[@entry]
let main (p : parameter) (x : storage) : operation list * storage =
  [],
  (match p with
  | Left i -> x - i
  | Right i -> x + i
  )
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
type storage = int;

type parameter =
   ["Left", int]
 | ["Right", int];

const main = (p: parameter, x: storage): [list<operation>, storage] =>
  [[],
   $match(p, {
    "Left": i => x - i,
    "Right": i => x + i
   })
  ];
```

</Syntax>

This contract can be called by another contract, like this one:

<Syntax syntax="cameligo">

```cameligo group=get_entrypoint_opt
type storage = int
type parameter = int

type x = Left of int

[@entry]
let main (p : parameter) (s : storage): operation list * storage =
  let contract =
    match Tezos.get_entrypoint_opt "%left" ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) with
    | Some c -> c
    | None -> failwith "contract does not match"
  in
  [Tezos.Operation.transaction (Left 2) 2mutez contract], s
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=get_entrypoint_opt
type storage = int;
type parameter = int;

type x = | ["Left", int];

// @entry
const main = (p: parameter, s: storage): [list<operation>, storage] => {
  const contract =
    $match(Tezos.get_entrypoint_opt("%left", "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"), {
      "Some": c => c,
      "None": () => failwith ("contract does not match")
    });
  return [[Tezos.Operation.transaction(["Left" as "Left", 2],
                                       2 as mutez, contract)], s];
};
```

</Syntax>

The calling contract uses the entrypoint with the `%left` annotation without mentioning the `%right` entrypoint.
LIGO uses the annotation to determine which side of the pair to put the `int` type into.

This currently only works for `or`'s and variant types in LIGO.

## Amendment

With amendment 007 to Tezos this is changed though, and also `pair`s
can be ordered differently.
