---
title: Maps
---

Maps are a data structure that associates keys to values.
Together, a key and its value make up a *binding*, also called an *element*.
The keys must all be the same type and the values must be the same type.
Keys must be unique within a map.

The predefined type `map` has two parameters: the first is the type of the keys, and the second is the type of the associated values.

Internally, LIGO sorts maps in increasing order by their keys.
Therefore, the type of the keys must be *comparable*, which means that Michelson allows them to be compared.
Most primitive types are comparable, including strings, ints, nats, and addresses.
To create more complex keys, you can use a [tuple](./tuples) of two comparable types.
For more information about Michelson types and which types are comparable, see [Michelson](https://octez.tezos.com/docs/active/michelson.html) in the Octez reference.

:::note Maps and big-maps

Maps are appropriate for small data sets and data sets that you want to load all at once, such as if you want to run logic on every element or check their lengths.
For data sets that may become larger, consider using a [Big-map](./big_maps).
Big-maps can be more efficient for larger data sets because only the elements that you access are loaded, which reduces gas fees.
However, this means that contracts can't do things that require them to load the entire big-map.

:::

## Creating maps

To create a map, you can use the predefined value `Map.empty` or create a non-empty map by passing a list of pairs of keys and values to the function `Map.literal`.
This example creates a map type that uses a string for the key and a list of strings for the value:

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

The `Map.literal` predefined function builds a map from a list of key-value pairs, `(<key>, <value>)`.
Note that each binding in the list is separated with a semicolon (`;`).

For reference, see the predefined [module Map](../reference/map-reference).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
type word       = string;
type definition = list<string>;
type dictionary = map<word, definition>;

const empty_dict: dictionary = Map.empty;

const dictionary: dictionary =
  Map.literal([
    ["one", (["The number 1.", "A member of a group."])],
    ["two", (["The number 2."])]]);
```

The `Map.literal` predefined function builds a map from a list of key-value pairs, `[<key>, <value>]`.

For reference, see the predefined [namespace Map](../reference/map-reference).

</Syntax>

## Sizing maps

The predefined function `Map.size` returns the number of bindings
(elements) in a given map.

<Syntax syntax="cameligo">

```cameligo group=map_size
let my_map : (int, string) map =
  Map.literal [(1,"one"); (2,"two")]
let size : nat = Map.size my_map // = 2
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_size
const my_map: map<int,string> =
  Map.literal([[1, "one"], [2, "two"]]);
const size: nat = Map.size(my_map); // == 2
```

</Syntax>

## Searching for elements

The predefined function `Map.mem` returns true if a value exists in the map for a given key.

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
  Map.literal([[1, "one"], [2, "two"]]);
const contains_2: bool = Map.mem(2, my_map); // == true
```

</Syntax>

To get the value for a key, use the `Map.find_opt` function, which returns an [option](./variants#options).
If the key exists in the map, the option is `Some()` with the value.
If the key does not exist in the map, the option is `None()`.

Because the return value of the `Map.find_opt` function is an option, you must account for missing keys in the map by [matching](./variants#matching) the return value, as in this example:

<Syntax syntax="cameligo">

```cameligo group=map_searching
let value_option : string option = Map.find_opt 2 my_map
let value key map =
  match Map.find_opt key map with
    Some value -> value
  | None -> failwith "No value."
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_searching
const value_option: option<string> = Map.find_opt(2, my_map);
const value = (key, map) =>
  $match(Map.find_opt(key, map), {
    "Some": (value) => value,
    "None": () => failwith("No value."),
  });
```

</Syntax>

As shorthand, you can use the function `Map.find`.
This function behaves like the previous example: it returns the value for a key if it exists or fails with the message `MAP FIND` if the value does not exist.

## Adding elements

To add an element to a map, pass the key and value to the `Map.add` function.
If the key already exists, the corresponding value is updated.

<Syntax syntax="cameligo">

```cameligo group=map_adding
let my_map : (int, string) map = Map.literal [(1, "one"); (2, "two")]
let new_map = Map.add 3 "three" my_map
let contains_3 = Map.mem 3 new_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_adding
const my_map: map<int,string> = Map.literal([[1, "one"], [2, "two"]]);
const new_map = Map.add(3, "three", my_map);
const contains_3 = Map.mem(3, new_map); // == true
```

</Syntax>

## Removing elements

The function `Map.remove` creates a map containing the elements of a given map, without the element with the given key.
If the element is not already present, the new map is the same as the old one.

<Syntax syntax="cameligo">

```cameligo group=map_removing
let my_map : (int, string) map = Map.literal [(1, "one"); (2, "two")]
let new_map = Map.remove 2 my_map
let contains_3 = Map.mem 2 new_map // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_removing
const my_map: map<int,string> = Map.literal([[1, "one"], [2, "two"]]);
const new_map = Map.remove(2, my_map);
const contains_3 = Map.mem(2, new_map); // == false
```

</Syntax>

## Updating elements

Previous sections show how to add and remove an element from a map.
The function `Map.update` can do both depending whether some value is given for the new binding or not.

To update a map in this way, pass the key and an option with the value.
If the option is `Some(value)`, the function adds the element, replacing any element with the given key.
If the option is `None()`, the function removes the element with the given key if it exists.
In either case, the function returns a new map, as in these examples:

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
const map_with_3 = Map.update (3, ["Some" as "Some", "three"], my_map);
const contains_3 = Map.mem(3, map_with_3); // == true
const map_without_2 = Map.update(2, ["None" as "None"], my_map);
const contains_2 = Map.mem(2, map_without_2); // == false
```

</Syntax>

To simultaneously update a map and obtain the value of the updated element, use the function `Map.get_and_update`.
This function allows you to extract a value from a map for use, as in this example:

<Syntax syntax="cameligo">

```cameligo group=map_updating
// three = Some "three"
let three, map_without_3 = Map.get_and_update 3 None map_with_3
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_updating
// three == ["Some" as "Some", "three"]
const [three, map_without_3] = Map.get_and_update(3, ["None" as "None"], map_with_3);
```

</Syntax>

## Working with maps as a whole

As described earlier, you can run logic on an entire map, but not a big-map.
LIGO runs logic on entire maps by applying a *functional iterator* to each element in the map.
In JsLIGO, you can also loop through the elements in a map, but this is not possible in CameLIGO.

### Folding maps

A map *fold*, known in some other languages as a *reduce*, runs the same function on each element in a map and returns a single value that is the result of those functions.
The function that you pass to the `Map.fold` function receives two arguments:

- The *accumulator*, which is the result of the previous function iteration
- The value of the current element

Each iteration of the function returns a new accumulator, which is passed to the next function.
The result of the last function iteration is the return value of the `Map.fold` function.
The `Map.fold` function iterates over the map in increasing order of its keys.

The `Map.fold` function accepts these parameters:

1. The fold function
1. The map to fold
1. The starting value for  the accumulator

For example, this code calculates the sum of the nats in a map.
At each iteration, the accumulator is the value of the sum of the elements up to that point.

<Syntax syntax="cameligo">

```cameligo group=map_folding
let my_map : (string, nat) map = Map.literal [
  ("Alice", 1n);
  ("Bob", 4n);
  ("Charlie", 5n);
]

let fold_function = fun (acc, element : nat * (string * nat)) ->
  let _key, value = element in
  acc + value

let map_sum = Map.fold fold_function my_map 0 (* 10 *)
```

For reference, see the predefined [module Map](../reference/map-reference).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_folding
const my_map: map<string, nat> = Map.literal([
  ["Alice", 1 as nat],
  ["Bob", 4 as nat],
  ["Charlie", 5 as nat],
]);

const fold_function = ([acc, element]: [nat, [string, nat]]): nat => {
  const [_key, value] = element;
  return acc + value;
}

const map_sum: nat = Map.fold(fold_function, my_map, 0 as nat); // 10 as nat
```

</Syntax>

### Mapping maps

The *mapping* operation (not to be confused with the map type itself) runs the same function on every value in a map and returns the resulting map.
Unlike folding, mapping operates on each element in the map independently from the others and returns a new map.

The function that you pass to the `Map.map` function receives the key and value of the current element and returns the new value for the same key.
You cannot change the key with this function; the new map has the same keys as the old map.

The following example takes a map of integers and squares each integer, producing a map with the same keys and the squared values:

<Syntax syntax="cameligo">

```cameligo group=map_mapping
let my_map : (string, int) map = Map.literal [
  ("Alice", 2);
  ("Bob", 5);
  ("Charlie", 8);
]

let squared_map : (string, int) map = Map.map (fun (_k, v : string * int) : int -> v * v) my_map
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_mapping
const my_map: map<string, int> = Map.literal([
  ["Alice", 2],
  ["Bob", 5],
  ["Charlie", 8],
]);

const squared_map: map<string, int> = Map.map(([_k, v]) => v * v, my_map);
```

</Syntax>

### Iterating over maps

An *iterated operation* is a fold over a map that returns the value of type `unit`, that is, its only use is to produce side-effects.
For example, iterating over maps can be useful if you want to verify that each element in a map meets certain criteria, and fail with an error otherwise.

To iterate over a map, pass the function to apply to each element to the `Map.iter` function.
This example iterates over a map of integers and fails if any of them are not greater than 3:

<Syntax syntax="cameligo">

```cameligo group=map_iterating
let assert_all_greater_than_3 (m : (int, int) map) : unit =
  Map.iter (fun (_, v) -> Assert.assert (v > 3)) m
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_iterating
const assert_all_greater_than_3 =
  (m: map<int,int>) : unit => Map.iter(([_k, v]) => Assert.assert(v > 3), m);
```
</Syntax>

## Looping

<Syntax syntax="cameligo">
There is no loop over maps in CameLIGO.

</Syntax>

<Syntax syntax="jsligo">

To iterate through all of the elements in a map, in increasing order of the keys, use the `for` loop in the form `for (const <variable> of <map>) <block>`.
In this loop, the `<block>` of statements (or a single statement) runs once for each `<variable>` ranging over the elements of the map `<map>` in increasing order.

Here is an example that adds the values in a map:

```jsligo group=map_looping
function sum_val (m: map<int, int>) {
  let sum = 0;
  // The key is discarded.
  for (const [_key, val] of m) sum = sum + val;
  return sum;
};
```

</Syntax>
