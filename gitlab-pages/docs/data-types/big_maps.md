---
title: Big maps
id: big_maps
---

import Syntax from '@theme/Syntax';

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary
maps.

For convenience, we duplicate here the documentation for maps in order
to feature big maps, and we point out any relevant differences:

  * The hash of the keys are internally used to access the entries of
    a big map, so this is advantageous when keys are large.

  * Since big maps are lazily-accessed maps, it makes no sense iterate
    over them. In particular, there is not equivalent to the function
    `Map.size`.

  * Big maps are not *packable*, as this would assume that they are
    entirely serialised first.

Big maps are a data structure which associates values of the same type
to values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

As a consequence, the predefined type `big_map` has two parameters:
the first is the type of the keys, and the second the type of the
associated values.

The empty big map is denoted by the predefined value
`Big_map.empty`. A non-empty big map can be built by using the
function `Big_map.literal` which takes a list of pairs of key and
values, and returns a big map containing them as bindings, and only
them.

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

The `Big_map.literal` predefined function builds a big map from a list
of key-value pairs, `(<key>, <value>)`.  Note also the "`;`" to
separate individual map bindings. Note that `("<string value>":
address)` means that we type-cast a string into an address.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
type word       = string;
type definition = list<string>;
type dictionary = big_map<word, definition>;

const empty_dict: dictionary = Big_map.empty;

const dictionary : dictionary =
  Big_map.literal([
    ["one", (["The number 1.", "A member of a group."] as definition)],
    ["two", (["The number 2."] as definition)]]);
```

The `Big_map.literal` predefined function builds a big map from a list
of key-value pairs, `[<key>, <value>]`.  Note also the "`,`" to
separate individual big map bindings. Note that `"<string value>" as
address` means that we type-cast a string into an address.

</Syntax>

> Note: Map keys are internally sorted by increasing values, so the
> type of the keys be *comparable*, that is, they obey a total order
> (any two keys can be compared).

## Searching

The predicate `Big_map.mem` tests for membership in a given big map,
given a purported key.

<Syntax syntax="cameligo">

```cameligo group=big_map_searching
let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Big_map.mem 2 my_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_searching
const my_map: big_map<int,string> =
  Big_map.literal([[1,"one"],[2,"two"]]);
const contains_2: bool = Big_map.mem(2, my_map); // == true
```

</Syntax>

In practice, however, we would like to get the value associated to the
key we searched. This is achieved by means of `Big_map.find_opt`.

<Syntax syntax="cameligo">

```cameligo group=big_map_searching
let v : string option = Big_map.find_opt 2 my_map
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_searching
const v : option<string> = Big_map.find_opt(2, my_map);
```

</Syntax>

Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the big map. This requires
*pattern matching*.

<Syntax syntax="cameligo">

```cameligo group=big_map_searching
let force_access key map =
  match Big_map.find_opt key map with
    Some value -> value
  | None -> failwith "No value."
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_searching
let force_access = (key, map) => {
  return match(Big_map.find_opt (key, map)) {
    when(Some(value)): value;
    when(None): failwith("No value.")
  };
};
```

</Syntax>

In fact, the predefined function `Big_map.find` does exactly that,
except that the exception raised by `failwith` carries the default
string `"MAP FIND"`.

## Adding

Adding a binding to a big map is done by calling the function
`Big_Map.add`. If the key was already present in the given big map,
the corresponding value is updated.

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

## Removing

The function `Big_map.remove` creates a big map containing the
elements of a given big map, without a given element. If the element
is not already present, the new big map is the same as the old one, as
expected.

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

## Updating

Previous sections show how to add and remove a binding from a given
big map. The function `Big_map.update` can do both depending whether
some value is given for the new binding or not: in the former case, a
new binding is added (and replaces any previous binding with the same
key); in the latter case, any binding with the same key is removed and
a new big map is returned.

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
const map_with_3 = Big_map.update (3, Some("three"), my_map);
const contains_3 = Big_map.mem(3, map_with_3); // == true
const map_without_2 = Big_map.update(2, None(), my_map);
const contains_2 = Big_map.mem (2, map_without_2); // == false
```

</Syntax>

When we want to update a big map, but also obtain the value of the
updated binding, we can use `Big_map.get_and_update`.

<Syntax syntax="cameligo">

```cameligo group=big_map_updating
// three = Some "three"
let three, map_without_3 = Big_map.get_and_update 3 None map_with_3
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map_updating
// three == Some("three")
const [three, map_without_3] =
  Big_map.get_and_update(3, None(), map_with_3);
```

</Syntax>
