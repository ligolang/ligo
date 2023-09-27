---
id: big-map-reference
title: Big_map
description: A lazily deserialised map that is intended to store large amounts of data.
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

A lazily deserialised map that is intended to store large amounts of
data. Here, "lazily" means that storage is read or written per key on
demand. Therefore there are no `map`, `fold`, and `iter` operations as
there are in [Map](map.md).

Compared to strict maps, which have a high upfront gas cost to
deserialise all the data and then have cheaper access costs
thereafter, lazily deserialised maps spread this cost out across each
access, increasing the per-access gas costs, but providing a cheaper
overall cost when only a small portion of a large dataset is needed.

<SyntaxTitle syntax="cameligo">
val empty : ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let empty: big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

Create an empty big map.

<Syntax syntax="cameligo">

```cameligo group=big_map
type move = int * int
type register = (address, move) big_map

let empty : register = Big_map.empty
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
type move = [int, int];
type register = big_map<address, move>;

let empty: register = Big_map.empty;
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val literal : ('key * 'value) list -> ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let literal: (items: list&lt;[&apos;key, &apos;value]&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

Create a non-empty big_map.

<Syntax syntax="cameligo">

```cameligo group=big_map
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
let moves: register =
  Big_map.literal (list([
    [("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address), [1, 2]],
    [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [0, 3]]]));
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val find_opt : 'key -> ('key, 'value) big_map -> 'value option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let find_opt: (key: &apos;key, big_map: big_map &lt;&apos;key, &apos;value&gt;) => option &lt;&apos;value&gt;
</SyntaxTitle>

Retrieve a value from a big map with the given key.

Because the key may be missing in the big map, the result is an
*optional value*.

<Syntax syntax="cameligo">

```cameligo group=big_map
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
let my_balance: option <move> =
  Big_map.find_opt(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val mem : 'key -> ('key, 'value) big_map -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let mem: (key: &apos;key, big_map: big_map &lt;&apos;key, &apos;value&gt;) => bool
</SyntaxTitle>

Test whether a given key exists within a big map.

<Syntax syntax="cameligo">

```cameligo group=big_map
let has_balance : bool =
  Big_map.mem ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
let has_balance: bool =
  Big_map.mem(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val update: 'key -> 'value option -> ('key, 'value) big_map -> ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let update: (key: &apos;key, value: option&lt;&apos;value&gt;, big_map: big_map&lt;&apos;key, &apos;value&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

Note: when `None` is used as a value, the value is removed from the big_map.

<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
let updated_map: register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), Some([4,9]), moves);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val get_and_update : 'key -> 'value option -> ('key, 'value) big_map -> value option * ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let get_and_update: (key: &apos;key, value: option&lt;&apos;value&gt;, big_map: big_map&lt;&apos;key, &apos;value&gt;) => [option&lt;&apos;value&gt;, big_map&lt;&apos;key, &apos;value&gt;]
</SyntaxTitle>

Similar to `update` but it also returns the value that was previously stored in the big_map

<SyntaxTitle syntax="cameligo">
val add : 'key -> 'value -> ('key, 'value) big_map  -> ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let add: (key: &apos;key, value: &apos;value, big_map: big_map&lt;&apos;key, &apos;value&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

```cameligo group=big_map
let add (m : register) : register =
  Big_map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
let add = (m: register): register =>
  Big_map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), [4,9], m);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val remove: 'key -> ('key, 'value) big_map -> ('key, 'value) big_map
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let remove: (key: &apos;key, big_map: big_map&lt;&apos;key, &apos;value&gt;) => big_map&lt;&apos;key, &apos;value&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Big_map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_map
let updated_map_: register =
  Big_map.remove(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address), moves);
```

</Syntax>
