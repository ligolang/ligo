---
id: map-reference
title: Map
---

## Map.find_opt(k: a', m: (a',b') map) : b' option

Retrieve the value associated with a particular key. This version returns an option
which can either shift logic in response to a missing value or throw an error.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const my_balance : option(move) = moves[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)];
```

<!--CameLIGO-->

```cameligo
let my_balance : move option = Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let my_balance : option(move) =
  Map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Map.find(k: a', m: (a', b') map) : b'

Forcefully retrieve the value associated with a particular key. If that value
doesn't exist, this function throws an error.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const my_balance : move = get_force(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves);
```

<!--CameLIGO-->

```cameligo
let my_balance : move = Map.find ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let my_balance : move =
  Map.find("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Map.update(k: a', v: b', m: (a', b') map) : (a', b') map

Change the value associated with a particular key, if that value doesn't already
exist add it.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

The values of a PascaLIGO map can be updated using the ordinary assignment syntax:

```pascaligo

function set_ (var m: moveset) : moveset is
  block {
    m[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9);
  } with m
```

<!--Cameligo-->

We can update a map in CameLIGO using the `Map.update` built-in:

```cameligo

let updated_map: moveset = Map.update ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) (Some (4,9)) moves
```

<!--Reasonligo-->

We can update a map in ReasonLIGO using the `Map.update` built-in:

```reasonligo

let updated_map: moveset = Map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some((4,9)), moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Map.add(k: a', v: b', m: (a', b') map) : (a', b') map

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function patch_ (var m: foobar) : foobar is block {
  patch m with map [0 -> 5; 1 -> 6; 2 -> 7]
} with m
```

<!--CameLIGO-->
```cameligo

```

<!--ReasonLIGO-->

<!--END_DOCUSAURUS_CODE_TABS-->

## Map.remove(k: a', m: (a', b') map) : (a', b') map

Remove a key and its associated value from the map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function rm (var m : foobar) : foobar is block {
  remove 42 from map m
} with m
```

<!--CameLIGO-->
```cameligo
let rm (m: foobar) : foobar = Map.remove 42 m
```

<!--ReasonLIGO-->
```reasonligo
let rm = (m: foobar): foobar => Map.remove(42, m);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Map.iter(iterator_function: (a', b') -> unit, m: (a', b') map) : unit

Run a function returning unit over the contents of a map's key-value pairs.
For example an assertion.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function iter_op (const m : moveset) : unit is
  block {
    function aggregate (const i : address ; const j : move) : unit is block
      { if j.1 > 1 then skip else failwith("fail") } with unit
  } with map_iter(aggregate, m);
```

<!--CameLIGO-->
```cameligo
let iter_op (m : moveset) : unit =
  let assert_eq = fun (i,j: address * move) -> assert (j.0 > 1)
  in Map.iter assert_eq m
```

<!--ReasonLIGO-->
```reasonligo
let iter_op = (m: moveset): unit => {
  let assert_eq = ((i,j): (address, move)) => assert (j[0] > 1);
  Map.iter(assert_eq, m);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Map.map(mapping_function: (a', b') -> b', m: (a', b') map) : (a', b') map

Update the values associated with every key in the map according to some update
rule `mapping_function`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function map_op (const m : moveset) : moveset is
  block {
    function increment (const i : address ; const j : move) : move is (j.0, j.1 + 1);
  } with map_map (increment, m);
```

<!--CameLIGO-->
```cameligo
let map_op (m : moveset) : moveset =
  let increment = fun (i,j: address * move) -> (j.0, j.1 + 1)
  in Map.map increment m
```

<!--ReasonLIGO-->
```reasonligo
let map_op = (m: moveset): moveset => {
  let increment = ((i,j): (address, move)) => (j[0], j[1] + 1);
  Map.map(increment, m);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Map.fold(folding_function: (b', (a', b')) -> b', m: (a', b') map, initial: b') : b'

Combine every value in the map together according to a fold rule `folding_function`. 

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function fold_op (const m : moveset) : int is
  block {
    function aggregate (const j : int; const cur : address * (int * int)) : int is j + cur.1.1
  } with map_fold(aggregate, m, 5)
```

<!--CameLIGO-->
```cameligo
let fold_op (m : moveset) : moveset =
  let aggregate = fun (i,j: int * (address * (int * int))) -> i + j.1.1
  in Map.fold aggregate m 5
```

<!--ReasonLIGO-->
```reasonligo
let fold_op = (m: moveset): moveset => {
  let aggregate = ((i,j): (int, (address, (int,int)))) => i + j[1][1];
  Map.fold(aggregate, m, 5);
};

```

<!--END_DOCUSAURUS_CODE_TABS-->


## Map.mem(k: a', m: (a', b') map) : bool

Test whether a particular key `k` exists in a given map `m`.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function mem (const k: int; const m: foobar) : bool is map_mem(k, m)
```
<!--CameLIGO-->
```cameligo
let mem (k,m: int * foobar) : bool = Map.mem k m
```

<!--ReasonLIGO-->
```reasonligo
let mem = (km: (int, foobar)): bool => Map.mem(km[0], km[1]);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Map.empty() : (a', b') map

Create an empty map.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
const empty_map : foobar = map end
```
<!--CameLIGO-->
```cameligo
let empty_map : foobar = Map.empty
```

<!--ReasonLIGO-->
```reasonligo
let empty_map: foobar = Map.empty;
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Map.literal(key_value_pair_list: (a', b') list) : (a', b') map

Constructs a map from a list of key-value pair tuples.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

```pascaligo
const moves: moveset = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> (1, 2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) -> (0, 3);
end
```

<!--CameLIGO-->

```cameligo
let moves: moveset = Map.literal
  [ (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address), (1, 2)) ;
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), (0, 3)) ;
  ]
```

<!--ReasonLIGO-->

```reasonligo
let moves : moveset =
  Map.literal([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address, (1, 2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, (0, 3)),
  ]);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Map.size(m: (a', b') map) : nat

Get the size of a given map `m`.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function size_ (const m : foobar) : nat is
  block {skip} with (size(m))
```
<!--CameLIGO-->
```cameligo
let size_ (m: foobar) : nat = Map.size m
```
<!--ReasonLIGO-->
```reasonligo
let size_ = (m: foobar): nat => Map.size(m);
```

<!--END_DOCUSAURUS_CODE_TABS-->
