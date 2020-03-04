---
id: map-reference
title: Maps
---

import Syntax from '@theme/Syntax';

*Maps* are a data structure which associate values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

# Declaring a Map



<Syntax syntax="pascaligo">

```pascaligo group=maps
type move is int * int
type register is map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
type move = int * int
type register = (address, move) map
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
type move = (int, int);
type register = map (address, move);
```

</Syntax>


# Creating an Empty Map



<Syntax syntax="pascaligo">

```pascaligo group=maps
const empty : register = map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let empty : register = Map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let empty : register = Map.empty
```

</Syntax>


# Creating a Non-empty Map


<Syntax syntax="pascaligo">

```pascaligo group=maps
const moves : register =
  map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let moves : register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let moves : register =
  Map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>


# Accessing Map Bindings


<Syntax syntax="pascaligo">

```pascaligo group=maps
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let my_balance : move option =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let my_balance : option (move) =
  Map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves);
```

</Syntax>


Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function force_access (const key : address; const moves : register) : move is
  case moves[key] of
    Some (move) -> move
  | None -> (failwith ("No move.") : move)
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let force_access (key, moves : address * register) : move =
  match Map.find_opt key moves with
    Some move -> move
  | None -> (failwith "No move." : move)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let force_access = ((key, moves) : (address, register)) : move => {
  switch (Map.find_opt (key, moves)) {
  | Some (move) => move
  | None => failwith ("No move.") : move
  }
};
```

</Syntax>


# Updating a Map

Given a map, we may want to add a new binding, remove one, or modify
one by changing the value associated to an already existing key. All
those operations are called *updates*.


<Syntax syntax="pascaligo">

```pascaligo group=maps
function assign (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=maps
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let assign (m : register) : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) m
```
Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
use `None` instead, that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```cameligo group=maps
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let assign = (m : register) : register =>
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), Some ((4,9)), m);
```

Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
use `None` instead, that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```reasonligo group=maps
let add = (m : register) : register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

</Syntax>


To remove a binding from a map, we need its key.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function delete (const key : address; var moves : register) : register is
  block {
    remove key from map moves
  } with moves
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let delete (key, moves : address * register) : register =
  Map.remove key moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let delete = ((key, moves) : (address, register)) : register =>
  Map.remove (key, moves);
```

</Syntax>



# Functional Iteration over Maps

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO maps: the
*iterated operation*, the *map operation* (not to be confused with the
*map data structure*) and the *fold operation*.

## Iterated Operation over Maps

The first, the *iterated operation*, is an iteration over the map with
no return value: its only use is to produce side-effects. This can be
useful if for example you would like to check that each value inside
of a map is within a certain range, and fail with an error otherwise.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function iter_op (const m : register) : unit is
  block {
    function iterated (const i : address; const j : move) : unit is
      if j.1 > 3 then Unit else (failwith ("Below range.") : unit)
  } with Map.iter (iterated, m)
```

> Note that `map_iter` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let iter_op = (m : register) : unit => {
  let predicate = ((i,j) : (address, move)) => assert (j[0] > 3);
  Map.iter (predicate, m);
};
```

</Syntax>


## Map Operations over Maps

We may want to change all the bindings of a map by applying to them a
function. This is called a *map operation*, not to be confused with
the map data structure. The predefined functional iterator
implementing the map operation over maps is called `Map.map`.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function map_op (const m : register) : register is
  block {
    function increment (const i : address; const j : move) : move is
      (j.0, j.1 + 1)
  } with Map.map (increment, m)
```

> Note that `map_map` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let map_op (m : register) : register =
  let increment = fun (i,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let map_op = (m : register) : register => {
  let increment = ((i,j): (address, move)) => (j[0], j[1] + 1);
  Map.map (increment, m);
};
```

</Syntax>


## Folded Operations over Maps

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.



<Syntax syntax="pascaligo">

```pascaligo group=maps
function fold_op (const m : register) : int is
  block {
    function folded (const i : int; const j : address * move) : int is
      i + j.1.1
  } with Map.fold (folded, m, 5)
```

> Note that `map_fold` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=maps
let fold_op (m : register) : register =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=maps
let fold_op = (m : register) : register => {
  let folded = ((i,j): (int, (address, move))) => i + j[1][1];
  Map.fold (folded, m, 5);
};
```

</Syntax>

