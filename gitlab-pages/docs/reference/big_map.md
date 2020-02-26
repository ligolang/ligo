---
id: big-map-reference
title: Big Maps — Scalable Maps
---

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary maps.

# Declaring a Map

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=big_maps
type move is int * int
type register is big_map (address, move)
```

<!--CameLIGO-->
```cameligo group=big_maps
type move = int * int
type register = (address, move) big_map
```

<!--ReasonLIGO-->
```reasonligo group=big_maps
type move = (int, int);
type register = big_map (address, move);
```
<!--END_DOCUSAURUS_CODE_TABS-->


# Creating an Empty Big Map

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=big_maps
const empty : register = big_map []
```

<!--CameLIGO-->
```cameligo group=big_maps
let empty : register = Big_map.empty
```

<!--ReasonLIGO-->
```reasonligo group=big_maps
let empty : register = Big_map.empty
```
<!--END_DOCUSAURUS_CODE_TABS-->

# Creating a Non-empty Map

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=big_maps
const moves : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

<!--CameLIGO-->
```cameligo group=big_maps
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

<!--ReasonLIGO-->
```reasonligo group=big_maps
let moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```
<!--END_DOCUSAURUS_CODE_TABS-->

# Accessing Values

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=big_maps
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

<!--CameLIGO-->
```cameligo group=big_maps
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

<!--ReasonLIGO-->
```reasonligo group=big_maps
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->


# Updating Big Maps

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=big_maps
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

<!--CameLIGO-->
```cameligo group=big_maps
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

<!--ReasonLIGO-->
```reasonligo group=big_maps
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some ((4,9)), moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

# Removing Bindings

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=big_maps
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

<!--CameLIGO-->
```cameligo group=big_maps
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->
```reasonligo group=big_maps
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

<!--END_DOCUSAURUS_CODE_TABS-->
