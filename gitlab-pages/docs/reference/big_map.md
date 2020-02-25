---
id: big-map-reference
title: Big Map — Scalable Maps
---

## Defining A Big Map Type

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=big_map
type move is int * int
type register is big_map (address, move)
```

<!--CameLIGO-->
```cameligo group=big_map
type move = int * int
type register = (address, move) big_map
```

<!--ReasonLIGO-->
```reasonligo group=big_map
type move = (int, int);
type register = big_map (address, move);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Creating an Empty Big Map

Create an empty big map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=big_map
const empty : register = big_map []
```

<!--CameLIGO-->
```cameligo group=big_map
let empty : register = Big_map.empty
```

<!--ReasonLIGO-->
```reasonligo group=big_map
let empty : register = Big_map.empty
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Populating A Big Map

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo group=big_map
const moves: register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

<!--CameLIGO-->

```cameligo group=big_map
let moves: register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

<!--ReasonLIGO-->

```reasonligo group=big_map
let moves: register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Retrieving a Value in a Big Map

Retrieve the value associated with a particular key. This version
returns an option which can either shift logic in response to a
missing value or throw an error.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=big_map
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

<!--CameLIGO-->

The function is `Big_map.find_opt` whose type is `'key ->
('key,'value) big_map) -> 'value option`. Recall that the function
`Big_map.find_opt` must always be fully applied to its arguments. Here
is an example:

```cameligo group=big_map
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

<!--ReasonLIGO-->

The function is `Big_map.find_opt` whose type is `('key, ('key,
'value) big_map) : 'value option`. Here is an example:

```reasonligo group=big_map
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Updating a Binding in a Big Map

Given a map, we may want to add a new binding, remove one, or modify
one by changing the value associated to an already existing key. We
may even want to retain the key but not the associated value. All
those operations are called *updates*.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

The values of a PascaLIGO big map can be updated using the ordinary
assignment syntax:

```pascaligo group=big_map
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

See further for the removal of bindings.

<!--Cameligo-->

In CameLIGO, you need the predefined function `Big_map.update` whose
type is `'key -> 'value option -> ('key, 'value) big_map -> ('key,
'value) big_map`. If the value (second argument) is `None`, then the
binding is to be removed. Recall that the function `Big_map.update`
must always be fully applied to its arguments. Here is an example:

```cameligo group=big_map
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

<!--Reasonligo-->

In ReasonLIGO, you need the predefined function `Big_map.update` whose
type is `('key, 'value option, ('key, 'value) big_map) : ('key,
'value) big_map`. If the value (second componenat) is `None`, then the
binding is to be removed. Here is an example:

```reasonligo group=big_map
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Adding a Binding to a Big Map

Add a key and its associated value to the big map.

<!--DOCUSAURUS_CODE_TABS-->

```pascaligo group=big_map
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

<!--CameLIGO-->

In CameLIGO, we use the predefined function `Big_map.add`, whose type
is `'key -> 'value -> ('key, 'value) big_map -> ('key, 'value)
big_map`. Recall that the function `Big_map.add` must always be fully
applied to its arguments. Here is an example:

```cameligo group=big_map
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

<!--ReasonLIGO-->

In ReasonLIGO, we use the predefined function `Big_map.add`, whose
type is `('key, 'value, ('key, 'value) big_map : ('key, 'value)
big_map`. Here is an example:

```reasonligo group=big_map
let add = (m : register) : register =>
  Map.add
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (4,9), m);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Removing a Binding from a Big Map

Remove a key and its associated value from the big map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=big_map
function delete (const key : address; var moves : register) : register is
  block {
    remove key from map moves
  } with moves
```

<!--CameLIGO-->

In CameLIGO, we use the predefined function `Big_map.remove` whose
type is `'key -> ('key, 'value) big_map -> ('key, 'value)
big_map`. Note that, despite being curried, the calls to that function
must be complete. Here is an example:

```cameligo group=big_map
let delete (key, moves : address * register) : register =
  Map.remove key moves
```

<!--ReasonLIGO-->

In ReasonLIGO, we use the predefined function `Big_map.remove` whose
type is `('key, ('key, 'value) big_map) : ('key, 'value)
big_map`. Here is an example:

```reasonligo group=big_map
let delete = ((key, moves) : (address, register)) : register =>
  Map.remove (key, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->
