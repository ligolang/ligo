---
id: big-map-reference
title: Big_map
description: A lazily deserialized map that's intended to store large amounts of data.
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

A lazily deserialized map that's intended to store large amounts of data.

The gast costs of deserialized maps are higher than standard maps as data is lazily deserialized.


<SyntaxTitle syntax="pascaligo">
type big_map (key, value)
</SyntaxTitle>


<SyntaxTitle syntax="cameligo">
type ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type big_map ('key, 'value)
</SyntaxTitle>

<Syntax syntax="pascaligo">

The type of a big map from values of type `key` to
values of type `value` is `big_map (key, value)`.

```pascaligo group=big_map
type move is int * int
type register is big_map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

The type of a big map from values of type `key` to values
of type `value` is `(key, value) big_map`.

```cameligo group=big_map
type move = int * int
type register = (address, move) big_map
```

</Syntax>
<Syntax syntax="reasonligo">
The type of a big map from values of type `key` to
values of type `value` is `big_map (key, value)`.

```reasonligo group=big_map
type move = (int, int);
type register = big_map (address, move);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function empty : big_map (key, value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val empty : ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let empty: big_map ('key, 'value)
</SyntaxTitle>

Create an empty big_map.

<Syntax syntax="pascaligo">

```pascaligo group=big_map
const empty : register = Big_map.empty
```

Alternatively, you can also create an empty big_map using:

```pascaligo group=big_map
const empty : register = big_map []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let empty : register = Big_map.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let empty : register = Big_map.empty
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function literal : list (key * value) -> big_map (key, value) 
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val literal : ('key * 'value) list -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let literal: list(('key, 'value)) => big_map('key, 'value)
</SyntaxTitle>

Create a non-empty big_map.

<Syntax syntax="pascaligo">

```pascaligo group=big_map
const moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)])
```

Alternative way of creating an empty big_map:

```pascaligo group=big_map
const moves : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function find_opt : key -> big_map (key, value) -> option value
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val find_opt : 'key -> ('key, 'value) big_map -> 'value option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let find_opt : ('key, big_map ('key, 'value)) => option ('value)
</SyntaxTitle>

Retrieve a value from a big map with the given key. 

Because the key may be missing in the big map, the result is an 
*optional value*.


<Syntax syntax="pascaligo">

```pascaligo group=big_map
const my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves)
```

Alternatively:

```pascaligo group=big_map
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function update : key -> option value -> big_map (key, value) -> big_map (key, value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val update: 'key -> 'value option -> ('key, 'value) big_map -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let update: ('key, option('value), big_map ('key, 'value)) => big_map ('key, 'value)
</SyntaxTitle>

Note: when `None` is used as a value, the value is removed from the big_map.

<Syntax syntax="pascaligo">

```pascaligo group=big_map
  Big_map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), m, (4,9));
```

Alternatively:

```pascaligo group=big_map
  m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=big_map
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

> Note the use of the keyword `map` instead of `big_map` (which is not
> a keyword).

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function add : key -> value -> big_map (key, value) -> big_map (key, value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val add : 'key -> 'value -> ('key, 'value) big_map  -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let add: ('key, 'value, big_map('key, 'value)) => big_map('key, 'value) 
</SyntaxTitle>
<Syntax syntax="pascaligo">

```pascaligo group=big_map
Big_map.add (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4, 9), m)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let add (m : register) : register =
  Big_map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let add = (m: register): register =>
  Big_map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
function remove: key -> big_map (key, value) -> big_map (key, value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val remove: 'key -> ('key, 'value) big_map -> ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let remove: (key, big_map ('key, 'value)) => big_map ('key, 'value)
</SyntaxTitle>

<Syntax syntax="pascaligo">

```pascaligo group=big_map
  const_updated_map : register = 
    Big_map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

Alternatively, the instruction `remove key from map m` removes the key
`key` from the big map `m` (note that the keyword is `map`, not
`big_map`).

```pascaligo group=big_map
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=big_map
let updated_map : register =
  Big_map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=big_map
let updated_map : register =
  Big_map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

</Syntax>

