---
title: Maps
---

*Maps* are a data structure which associates values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

As a consequence, the predefined type `map` has two parameters: the
first is the type of the keys, and the second the type of the
associated values.

The empty map is denoted by the predefined value `Map.empty`. A
non-empty map can be built by using the function `Map.literal` which
takes a list of pairs of key and values, and returns a map containing
them as bindings, and only them.

<Syntax syntax="cameligo">

```cameligo group=maps
type word       = string
type definition = string list
type dictionary = (word, definition) map

let empty_dict : dictionary = Map.empty

let dictionary : dictionary =
  Map.literal [
    ("one", ["The number 1."; "A member of a group."]);
    ("two", ["The number 2"])]
```

The `Map.literal` predefined function builds a map from a list of
key-value pairs, `(<key>, <value>)`.  Note also the "`;`" to separate
individual map bindings. Note that `("<string value>": address)` means
that we type-cast a string into an address.

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
type word       = string;
type definition = list<string>;
type dictionary = map<word, definition>;

const empty_dict: dictionary = Map.empty;

const dictionary : dictionary =
  Map.literal([
    ["one", (["The number 1.", "A member of a group."] as definition)],
    ["two", (["The number 2."] as definition)]]);
```

The `Map.literal` predefined function builds a map from a list of
key-value pairs, `[<key>, <value>]`.  Note also the "`,`" to separate
individual map bindings. Note that `"<string value>" as address` means
that we type-cast a string into an address.

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

> Note: Map keys are internally sorted by increasing values, so the
> type of the keys be *comparable*, that is, they obey a total order
> (any two keys can be compared).

## Sizing

The predefined function `Map.size` returns the number of bindings
(entries) in a given map.

<Syntax syntax="cameligo">

```cameligo group=map_size
let my_map : (int, string) map =
  Map.literal [(1,"one"); (2,"two")]
let size : nat = Map.size my_map // = 2
```
Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_size
const my_map: map<int,string> =
  Map.literal([[1,"one"],[2,"two"]]);
const size: nat = Map.size(my_map); // == 2
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Searching

The predicate `Map.mem` tests for membership in a given map, given a
purported key.

<Syntax syntax="cameligo">

```cameligo group=map_searching
let my_map : (int, string) map =
  Map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Map.mem 2 my_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_searching
const my_map: map<int,string> =
  Map.literal([[1,"one"],[2,"two"]]);
const contains_2: bool = Map.mem(2, my_map); // == true
```

</Syntax>

In practice, however, we would like to get the value associated to the
key we searched. This is achieved by means of `Map.find_opt`.

<Syntax syntax="cameligo">

```cameligo group=map_searching
let v : string option = Map.find_opt 2 my_map
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_searching
const v : option<string> = Map.find_opt(2, my_map);
```

</Syntax>

Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.

<Syntax syntax="cameligo">

```cameligo group=map_searching
let force_access key map =
  match Map.find_opt key map with
    Some value -> value
  | None -> failwith "No value."
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_searching
let force_access = (key, map) => {
  return match(Map.find_opt (key, map)) {
    when(Some(value)): value;
    when(None): failwith("No value.")
  };
};
```

</Syntax>

In fact, the predefined function `Map.find` does exactly that, except
that the exception raised by `failwith` carries the default string
`"MAP FIND"`.

<Syntax syntax="cameligo">

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Adding

Adding a binding to a map is done by calling the function
`Map.add`. If the key was already present in the given map, the
corresponding value is updated.

<Syntax syntax="cameligo">

```cameligo group=map_adding
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let new_map = Map.add 3 "three" my_map
let contains_3 = Map.mem 3 new_map // = true
```

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_adding
const my_map : map<int,string> = Map.literal([[1,"one"],[2,"two"]]);
const new_map = Map.add(3, "three", my_map);
const contains_3 = Map.mem(3, new_map); // == true
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Removing

The function `Map.remove` creates a map containing the elements of a
given map, without a given element. If the element is not already
present, the new map is the same as the old one, as expected.

<Syntax syntax="cameligo">

```cameligo group=map_removing
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let new_map = Map.remove 2 my_map
let contains_3 = Map.mem 2 new_map // = false
```

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_removing
const my_map : map<int,string> = Map.literal([[1,"one"],[2,"two"]]);
const new_map = Map.remove(2, my_map);
const contains_3 = Map.mem(2, new_map); // == false
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Updating

Previous sections show how to add and remove a binding from a given
map. The function `Map.update` can do both depending whether some
value is given for the new binding or not: in the former case, a new
binding is added (and replaces any previous binding with the same
key); in the latter case, any binding with the same key is removed and
a new map is returned.

<Syntax syntax="cameligo">

```cameligo group=map_updating
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let map_with_3 = Map.update 3 (Some "three") my_map
let contains_3 = Map.mem 3 map_with_3 // = true
let map_without_2 = Map.update 2 None my_map
let contains_2 = Map.mem 2 map_without_2 // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_updating
const my_map: map<int,string> = Map.literal([[1,"one"],[2,"two"]]);
const map_with_3 = Map.update (3, Some("three"), my_map);
const contains_3 = Map.mem(3, map_with_3); // == true
const map_without_2 = Map.update(2, None(), my_map);
const contains_2 = Map.mem (2, map_without_2); // == false
```

</Syntax>

When we want to update a map, but also obtain the value of the updated
binding, we can use `Map.get_and_update`.

<Syntax syntax="cameligo">

```cameligo group=map_updating
// three = Some "three"
let three, map_without_3 = Map.get_and_update 3 None map_with_3
```

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_updating
// three == Some("three")
const [three, map_without_3] = Map.get_and_update(3, None(), map_with_3);
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Folding

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is sometimes possible:
*loops*.

There are three kinds of functional iterations over maps: the *fold*,
the *map* (not to be confused with the *map data structure*) and the
*iteration*.

Let us consider first here the fold, which is the most general form of
functional iteration. The folded function takes two arguments: an
*accumulator* and the structure *element* at hand, with which it then
produces a new accumulator. This enables having a partial result that
becomes complete when the traversal of the data structure is over.

The function `Map.fold` performs a fold over the binding of a map, in
increasing order of its keys.

<Syntax syntax="cameligo">

```cameligo group=map_folding
type player = string
type abscissa = int
type ordinate = int
type move = abscissa * ordinate
type game = (player, move) map

let horizontal_offset (g : game) : int =
  let folded = fun (acc, j : int * (player * move)) -> acc + j.1.0
  in Map.fold folded g 0
```

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_folding
type player = string
type abscissa = int
type ordinate = int
type move = [abscissa, ordinate]
type game = map<player, move>

const horizontal_offset = (g: game): int => {
  let folded = ([acc, j]: [int, [player, move]]) => acc + j[1][0];
  return Map.fold(folded, g, 0);
};
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Mapping

We may want to change all the values of a given map by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over maps is called `Map.map`. It
takes a binding, that is, a key and its associated value in the map,
and computes a new value for that key.

In the following example, from a map from integers to integers is made
a map whose values are the sum of the keys and values of each binding.

<Syntax syntax="cameligo">

```cameligo group=map_mapping
let my_map : (int, int) map = Map.literal [(0,0); (1,1); (2,2)]
// plus_one = Map.literal [(0,0); (1,2); (2,4)]
let plus_one = Map.map (fun (k,v) -> k + v) my_map
```

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_mapping
const my_map : map<int,int> = Map.literal([[0,0], [1,1], [2,2]]);
// plus_one == Map.literal([[0,0],[1,2],[2,4]])
const plus_one = Map.map(([k,v]) => k + v, my_map);
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Iterating

An *iterated operation* is a fold over the map that returns the value
of type `unit`, that is, its only use is to produce side-effects. This
can be useful if, for example, you would like to check that each value
of a map is within a certain range, and fail with an error otherwise.

The predefined functional iterator implementing the iterated operation
over maps is called `Map.iter`. It
takes a binding, that is, a key and its associated value in the map,
performs some side-effect and returns the unit value.

In the following example, a map is iterated to check that all its
integer values are greater than `3`.

<Syntax syntax="cameligo">

```cameligo group=map_iterating
let assert_all_greater_than_3 (m : (int, int) map) : unit =
  Map.iter (fun (_,v) -> assert (v > 3)) m  // The key is discarded
```

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_iterating
const assert_all_greater_than_3 =
  (m: map<int,int>) : unit => Map.iter(([_k,v]) => assert(v > 3), m);
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>

## Looping

<Syntax syntax="cameligo">
There is no loop over maps in CameLIGO.

Note: See the predefined
[module Map](../reference/map-reference)

</Syntax>

<Syntax syntax="jsligo">

One can iterate through all the bindings of a map, in increasing order
of the keys, thanks to a loop of the form `for (const <variable> of <map>) <block>`. It means that the `<block>` of statements (or a
single statement) will be computed once for each `<variable>` ranging
over the bindings (as pairs of keys and values) of the map `<map>` in
increasing order.

Here is an example where the values in a map are summed up.

```jsligo group=map_looping
function sum_val (m: map<int,int>) {
  let sum = 0;
  // The key is discarded.
  for (const [_key, val] of m) sum = sum + val;
  return sum;
};
```

Note: See the predefined
[namespace Map](../reference/map-reference)

</Syntax>
