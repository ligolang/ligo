---
title: Big maps
id: big_maps
---

import Syntax from '@theme/Syntax';

Big-maps are data structures that are similar to [Maps](./maps) but are optimized in Michelson for large datasets.
Like maps, big-maps associate keys to values, where all keys are the same type and all values are the same type.
Like maps, the keys must be comparable types, which includes most primitives and tuples consisting of otherwise comparable types.

As described in [Maps](./maps), to read a single element from a map, the contract must load the entire map.
By contrast, elements in big-maps are lazily deserialised, which means that when a contract reads elements from a big-map, it loads only the elements that it needs to access, instead of the entire big-map.
Loading only the necessary elements makes big-maps more cost-effective than maps when the dataset gets large because the contract manipulates less data.
Internally, big-maps also store their keys as hashes instead of raw data, which can save space when the keys are large or complex.

Despite these internal differences, big-maps behave much like maps in contracts.
However, the way big-maps are stored and deserialised causes these limitations that maps don't have:

- You can't get the number of elements in a big-map like you can with the `Map.size` function.
- There is no way to get a list of all of the keys in a big-map.
- You can't iterate through the elements in a big-map or use functions that access all of the elements in a big-map like the `Map.fold` and `Map.map` functions.
- Big-maps are not packable.

## Creating big-maps

To create a big-map, you can use the predefined value `Big_map.empty` or create a non-empty map by passing a list of pairs of keys and values to the function `Big_map.literal`.
This example creates a big-map type that uses a string for the key and a list of strings for the value:

<Syntax syntax="cameligo">

```cameligo group=big_maps
type word       = string
type definition = string list
type dictionary = (word, definition) big_map

let empty_dict : dictionary = Big_map.empty

let dictionary : dictionary =
  Big_map.literal [
    ("one", ["The number 1."; "A member of a group."]);
    ("two", ["The number 2"])]
```

The `Big_map.literal` predefined function builds a big-map from a list of key-value pairs, `(<key>, <value>)`.
Note that each binding in the list is separated with a semicolon (`;`).

For reference, see the predefined [module Big_map](../reference/big-map-reference).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
type word       = string;
type definition = list<string>;
type dictionary = big_map<word, definition>;

const empty_dict: dictionary = Big_map.empty;

const dictionary: dictionary =
  Big_map.literal([
    ["one", (["The number 1.", "A member of a group."] as definition)],
    ["two", (["The number 2."] as definition)]]);
```

The `Big_map.literal` predefined function builds a big-map from a list of key-value pairs, `[<key>, <value>]`.
Note that each binding in the list is separated with a comma (`,`).

For reference, see the predefined [namespace Big_map](../reference/big-map-reference).

</Syntax>

## Searching for elements

The predefined function `Big_map.mem` returns true if a value exists in the big-map for a given key.

<Syntax syntax="cameligo">

```cameligo group=big_map_searching
let my_big_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Big_map.mem 2 my_big_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_searching
const my_big_map: big_map<int,string> =
  Big_map.literal([[1, "one"], [2, "two"]]);
const contains_2: bool = Big_map.mem(2, my_big_map); // == true
```

</Syntax>

To get the value for a key, use the `Big_map.find_opt` function, which returns an [option](./variants#options).
If the key exists in the big-map, the option is `Some()` with the value.
If the key does not exist in the big-map, the option is `None()`.

Because the return value of the `Big_map.find_opt` function is an option, you must account for missing keys in the big-map by [matching](./variants#matching) the return value, as in this example:

<Syntax syntax="cameligo">

```cameligo group=big_map_searching
let value_option : string option = Big_map.find_opt 2 my_big_map
let value = match value_option with
    Some value -> value
  | None -> failwith "No value."
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_searching
const value_option: option<string> = Big_map.find_opt(2, my_big_map);
const value = $match(value_option, {
  "Some": value => value,
  "None": () => failwith("No value."),
});
```

</Syntax>

As shorthand, you can use the function `Big_map.find`.
This function behaves like the previous example: it returns the value for a key if it exists or fails with the message `MAP FIND` if the value does not exist.

## Adding elements

To add an element to a big-map, pass the key and value to the `Big_map.add` function.
If the key already exists, the corresponding value is updated.

<Syntax syntax="cameligo">

```cameligo group=big_map_adding
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let new_map = Big_map.add 3 "three" my_map
let contains_3 = Big_map.mem 3 new_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_adding
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const new_map = Big_map.add(3, "three", my_map);
const contains_3 = Big_map.mem(3, new_map); // == true
```

</Syntax>

## Removing elements

The function `Big_map.remove` creates a big-map containing the elements of a given big-map, without the element with the given key.
If the element is not already present, the new big-map is the same as the old one.

<Syntax syntax="cameligo">

```cameligo group=big_map_removing
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let new_map = Big_map.remove 2 my_map
let contains_3 = Big_map.mem 2 new_map // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_removing
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const new_map = Big_map.remove(2, my_map);
const contains_3 = Big_map.mem(2, new_map); // == false
```

</Syntax>

## Updating elements

Previous sections show how to add and remove an element from a big-map.
The function `Big_map.update` can do both depending whether some value is given for the new binding or not.

To update a big-map in this way, pass the key and an option with the value.
If the option is `Some(value)`, the function adds the element, replacing any element with the given key.
If the option is `None()`, the function removes the element with the given key if it exists.
In either case, the function returns a new big-map, as in these examples:

<Syntax syntax="cameligo">

```cameligo group=big_map_updating
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let map_with_3 = Big_map.update 3 (Some "three") my_map
let contains_3 = Big_map.mem 3 map_with_3 // = true
let map_without_2 = Big_map.update 2 None my_map
let contains_2 = Big_map.mem 2 map_without_2 // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_updating
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const map_with_3 = Big_map.update (3, ["Some" as "Some", "three"], my_map);
const contains_3 = Big_map.mem(3, map_with_3); // == true
const map_without_2 = Big_map.update(2, ["None" as "None"], my_map);
const contains_2 = Big_map.mem (2, map_without_2); // == false
```

</Syntax>

To simultaneously update a map and obtain the value of the updated element, use the function `Big_map.get_and_update`.
This function allows you to extract a value from a big-map for use, as in this example:

<Syntax syntax="cameligo">

```cameligo group=big_map_updating
// three = Some "three"
let three, map_without_3 = Big_map.get_and_update 3 None map_with_3
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_updating
// three == ["Some" as "Some", "three"]
const [three, map_without_3] =
  Big_map.get_and_update(3, ["None" as "None"], map_with_3);
```

</Syntax>
