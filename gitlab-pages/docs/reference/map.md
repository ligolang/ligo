---
id: map-reference
title: Map
---

## Defining A Map Type

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=map
type move is int * int
type register is map (address, move)
```

<!--CameLIGO-->
```cameligo group=map
type move = int * int
type register = (address, move) map
```

<!--ReasonLIGO-->
```reasonligo group=map
type move = (int, int);
type register = map (address, move);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Creating an Empty Map

Create an empty map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=map
const empty : register = map []
```

<!--CameLIGO-->
```cameligo group=map
let empty : register = Map.empty
```

<!--ReasonLIGO-->
```reasonligo group=map
let empty : register = Map.empty
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Populating A Map

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo group=map
const moves: register =
  map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

<!--CameLIGO-->

```cameligo group=map
let moves: register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

<!--ReasonLIGO-->

```reasonligo group=map
let moves: register =
  Map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Retrieving a Value in a Map

Retrieve the value associated with a particular key. This version
returns an option which can either shift logic in response to a
missing value or throw an error.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=map
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

<!--CameLIGO-->

The function is `Map.find_opt` whose type is `'key ->
('key,'value) map) -> 'value option`. Recall that the function
`Map.find_opt` must always be fully applied to its arguments. Here
is an example:

```cameligo group=map
let my_balance : move option =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

<!--ReasonLIGO-->

The function is `Map.find_opt` whose type is `('key, ('key,
'value) map) : 'value option`. Here is an example:

```reasonligo group=map
let my_balance : option (move) =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Updating a Binding in a Map

Given a map, we may want to add a new binding, remove one, or modify
one by changing the value associated to an already existing key. We
may even want to retain the key but not the associated value. All
those operations are called *updates*.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

The values of a PascaLIGO map can be updated using the ordinary
assignment syntax:

```pascaligo group=map
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

See further for the removal of bindings.

<!--Cameligo-->

In CameLIGO, you need the predefined function `Map.update` whose
type is `'key -> 'value option -> ('key, 'value) map -> ('key,
'value) map`. If the value (second argument) is `None`, then the
binding is to be removed. Recall that the function `Map.update`
must always be fully applied to its arguments. Here is an example:

```cameligo group=map
let updated_map : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

<!--Reasonligo-->

In ReasonLIGO, you need the predefined function `Map.update` whose
type is `('key, 'value option, ('key, 'value) map) : ('key,
'value) map`. If the value (second componenat) is `None`, then the
binding is to be removed. Here is an example:

```reasonligo group=map
let updated_map : register =
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Adding a Binding to a Map

Add a key and its associated value to the map.

<!--DOCUSAURUS_CODE_TABS-->

```pascaligo group=map
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

<!--CameLIGO-->

In CameLIGO, we use the predefined function `Map.add`, whose type
is `'key -> 'value -> ('key, 'value) map -> ('key, 'value)
map`. Recall that the function `Map.add` must always be fully
applied to its arguments. Here is an example:

```cameligo group=map
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

<!--ReasonLIGO-->

In ReasonLIGO, we use the predefined function `Map.add`, whose
type is `('key, 'value, ('key, 'value) map : ('key, 'value)
map`. Here is an example:

```reasonligo group=map
let add = (m : register) : register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Removing a Binding from a Map

Remove a key and its associated value from the map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=map
function delete (const key : address; var moves : register) : register is
  block {
    remove key from map moves
  } with moves
```

<!--CameLIGO-->

In CameLIGO, we use the predefined function `Map.remove` whose
type is `'key -> ('key, 'value) map -> ('key, 'value)
map`. Note that, despite being curried, the calls to that function
must be complete. Here is an example:

```cameligo group=map
let delete (key, moves : address * register) : register =
  Map.remove key moves
```

<!--ReasonLIGO-->

In ReasonLIGO, we use the predefined function `Map.remove` whose
type is `('key, ('key, 'value) map) : ('key, 'value)
map`. Here is an example:

```reasonligo group=map
let delete = ((key, moves) : (address, register)) : register =>
  Map.remove (key, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Getting the Size of a Map

The size of a map is its number of bindings.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=map
function size_of (const m : register) : nat is size (m)
```
<!--CameLIGO-->
```cameligo group=map
let size_of (m : register) : nat = Map.size m
```
<!--ReasonLIGO-->
```reasonligo group=map
let size_of = (m : register): nat => Map.size (m);
```

<!--END_DOCUSAURUS_CODE_TABS-->
