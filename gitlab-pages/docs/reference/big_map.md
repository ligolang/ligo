---
id: big-map-reference
title: Big Map
---

## Big_map.find_opt(k: a', m: (a',b') big_map) : b' option

Retrieve the value associated with a particular key. This version returns an option
which can either shift logic in response to a missing value or throw an error.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const my_balance : option(move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)]
```

<!--CameLIGO-->

```cameligo
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let my_balance : option(move) =
  Big_map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Big_map.find(k: a', m: (a', b') big_map) : b'

Forcefully retrieve the value associated with a particular key. If that value
doesn't exist, this function throws an error.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const my_balance : move =
  get_force (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves);
```

<!--CameLIGO-->

```cameligo
let my_balance : move =
  Big_map.find ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let my_balance : move =
  Big_map.find ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Big_map.update(k: a', v: b', m: (a', b') big_map) : (a', b') big_map

Change the value associated with a particular key, if that value doesn't already
exist add it.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

The values of a PascaLIGO big map can be updated using the ordinary
assignment syntax:

```pascaligo

function set_ (var m : moveset) : moveset is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9);
  } with m
```

<!--Cameligo-->

```cameligo
let updated_map : moveset =
  Big_map.update ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) (Some (4,9)) moves
```

<!--Reasonligo-->

```reasonligo
let updated_map : moveset =
  Big_map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some((4,9)), moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Big_map.add(k: a', v: b', m: (a', b') big_map) : (a', b') big_map

## Big_map.remove(k: a', m: (a', b') big_map) : (a', b') big_map

Remove a key and its associated value from the big map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function rm (var m : foo) : foo is block {
  remove 42 from map m;
} with m
```

<!--CameLIGO-->
```cameligo
let rm (m : foo) : foo = Big_map.remove 42 m
```

<!--ReasonLIGO-->
```reasonligo
let rm = (m: foo): foo => Big_map.remove(42, m);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Big_map.literal(key_value_pair_list: (a', b') list) : (a', b') big_map

Constructs a big map from a list of key-value pair tuples.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
const moves: moveset =
  big_map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) -> (0,3);
  end
```

<!--CameLIGO-->

```cameligo
let moves: moveset =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), (0,3));
  ]
```

<!--ReasonLIGO-->

```reasonligo
let moves: moveset =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, (0,3)),
  ]);
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Big_map.empty() : (a', b') big_map

Create an empty big map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
const empty_big_map : big_map(int,int) = big_map end
```

<!--CameLIGO-->
```cameligo
let empty_map : foo = Big_map.empty
```

<!--ReasonLIGO-->
```reasonligo
let empty_map: foo = Big_map.empty;
```

<!--END_DOCUSAURUS_CODE_TABS-->

