---
id: searching
title: Searching
---

import Syntax from '@theme/Syntax';

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
