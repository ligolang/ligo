---
id: searching
title: Searching
---

import Syntax from '@theme/Syntax';

The predicate `Map.mem` tests for membership in a given map, given a
purported key.

<Syntax syntax="cameligo">

```cameligo group=map_searching
let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Map.mem 2 my_map // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_searching
const my_map: map<int,string> = Map.literal(list([[1,"one"],[2,"two"]]));
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

```jsligo group=maps
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
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
