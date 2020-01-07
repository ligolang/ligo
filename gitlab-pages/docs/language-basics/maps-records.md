---
id: maps-records
title: Maps, Records
---

So far we've seen pretty basic data types. LIGO also offers more complex built-in constructs, such as Maps and Records.

## Maps

Maps are natively available in Michelson, and LIGO builds on top of them. A requirement for a Map is that its keys be of the same type, and that type must be comparable.

Here's how a custom map type is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type move is (int * int);
type moveset is map(address, move);
```

<!--CameLIGO-->
```cameligo
type move = int * int
type moveset = (address, move) map
```

<!--ReasonLIGO-->
```reasonligo
type move = (int, int);
type moveset = map(address, move);
```

<!--END_DOCUSAURUS_CODE_TABS-->

And here's how a map value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
const moves: moveset = map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> (1, 2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) -> (0, 3);
end
```
> Notice the `->` between the key and its value and `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--CameLIGO-->

```cameligo
let moves: moveset = Map.literal
  [ (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address), (1, 2)) ;
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), (0, 3)) ;
  ]
```
> Map.literal constructs the map from a list of key-value pair tuples, `(<key>, <value>)`.
> Note also the `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--ReasonLIGO-->

```reasonligo
let moves: moveset =
  Map.literal([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address, (1, 2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, (0, 3)),
  ]);
```
> Map.literal constructs the map from a list of key-value pair tuples, `(<key>, <value>)`.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing map values by key

If we want to access a move from our moveset above, we can use the `[]` operator/accessor to read the associated `move` value. However, the value we'll get will be wrapped as an optional; in our case `option(move)`. Here's an example:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: option(move) = moves[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)];
```

<!--CameLIGO-->

```cameligo
let balance: move option = Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let balance: option(move) =
  Map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### Obtaining a map value forcefully

Accessing a value in a map yields an option, however you can also get the value directly:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: move = get_force(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves);
```

<!--CameLIGO-->

```cameligo
let balance: move = Map.find ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let balance: move =
  Map.find("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Updating the contents of a map

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


### Iteration over the contents of a map

There are three kinds of iteration on LIGO maps, `iter`, `map` and `fold`. `iter`
is an iteration over the map with no return value, its only use is to
generate side effects. This can be useful if for example you would like to check
that each value inside of a map is within a certain range, with an error thrown
otherwise.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function iter_op (const m : moveset) : unit is
  block {
    function aggregate (const i : address ; const j : move) : unit is block
      { if (j.1 > 1) then skip else failwith("fail") } with unit ;
  } with map_iter(aggregate, m) ;
```

<!--CameLIGO-->
```cameligo
let iter_op (m : moveset) : unit =
  let assert_eq = fun (i: address) (j: move) -> assert (j.0 > 1)
  in Map.iter assert_eq m
```

<!--ReasonLIGO-->
```reasonligo
let iter_op = (m: moveset): unit => {
  let assert_eq = (i: address, j: move) => assert(j[0] > 1);
  Map.iter(assert_eq, m);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

`map` is a way to create a new map by modifying the contents of an existing one.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function map_op (const m : moveset) : moveset is
  block {
    function increment (const i : address ; const j : move) : move is block { skip } with (j.0, j.1 + 1) ;
  } with map_map(increment, m) ;
```

<!--CameLIGO-->
```cameligo
let map_op (m : moveset) : moveset =
  let increment = fun (_: address) (j: move) -> (j.0, j.1 + 1)
  in Map.map increment m
```

<!--ReasonLIGO-->
```reasonligo
let map_op = (m: moveset): moveset => {
  let increment = (ignore: address, j: move) => (j[0], j[1] + 1);
  Map.map(increment, m);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

`fold` is an aggregation function that return the combination of a maps contents.

The fold is a loop which extracts an element of the map on each iteration. It then
provides this element and an existing value to a folding function which combines them.
On the first iteration, the existing value is an initial expression given by the
programmer. On each subsequent iteration it is the result of the previous iteration.
It eventually returns the result of combining all the elements.


<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function fold_op (const m : moveset) : int is
  block {
    function aggregate (const j : int ; const cur : (address * (int * int))) : int is j + cur.1.1 ;
  } with map_fold(aggregate, m , 5)
```

<!--CameLIGO-->
```cameligo
let fold_op (m : moveset) : moveset =
  let aggregate = fun (j: int) (cur: address * (int * int)) -> j + cur.1.1 in
  Map.fold aggregate m 5
```

<!--ReasonLIGO-->
```reasonligo
let fold_op = (m: moveset): moveset => {
  let aggregate = (j: int, cur: (address, (int,int))) => j + cur[1][1];
  Map.fold(aggregate, m, 5);
};

```

<!--END_DOCUSAURUS_CODE_TABS-->

## Big Maps

Ordinary maps are fine for contracts with a finite lifespan or a bounded number
of users. For many contracts however, the intention is to have a map hold *many*
entries, potentially millions or billions. The cost of loading these entries into
the environment each time a user executes the contract would eventually become
too expensive were it not for big maps. Big maps are a data structure offered by
Tezos which handles the scaling concerns for us. In LIGO, the interface for big
maps is analogous to the one used for ordinary maps. 

Here's how we define a big map:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type move is (int * int);
type moveset is big_map(address, move);
```

<!--CameLIGO-->
```cameligo
type move = int * int
type moveset = (address, move) big_map
```

<!--ReasonLIGO-->
```reasonligo
type move = (int, int);
type moveset = big_map(address, move);
```

<!--END_DOCUSAURUS_CODE_TABS-->

And here's how a map value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

```pascaligo
const moves: moveset = big_map
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) -> (1, 2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) -> (0, 3);
end
```
> Notice the `->` between the key and its value and `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--CameLIGO-->

```cameligo
let moves: moveset = Big_map.literal
  [ (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address), (1, 2)) ;
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), (0, 3)) ;
  ]
```
> Big_map.literal constructs the map from a list of key-value pair tuples, `(<key>, <value>)`.
> Note also the `;` to separate individual map entries.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--ReasonLIGO-->

```reasonligo
let moves: moveset =
  Big_map.literal([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address, (1, 2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, (0, 3)),
  ]);
```
> Big_map.literal constructs the map from a list of key-value pair tuples, `(<key>, <value>)`.
>
> `("<string value>": address)` means that we type-cast a string into an address.

<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing map values by key

If we want to access a move from our moveset above, we can use the `[]` operator/accessor to read the associated `move` value. However, the value we'll get will be wrapped as an optional; in our case `option(move)`. Here's an example:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: option(move) = moves[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)];
```

<!--CameLIGO-->

```cameligo
let balance: move option = Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let balance: option(move) =
  Big_map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### Obtaining a map value forcefully

Accessing a value in a map yields an option, however you can also get the value directly:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const balance: move = get_force(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves);
```

<!--CameLIGO-->

```cameligo
let balance: move = Big_map.find ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

```reasonligo
let balance: move = Big_map.find("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Updating the contents of a big map

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

The values of a PascaLIGO big map can be updated using the ordinary assignment syntax:

```pascaligo

function set_ (var m: moveset) : moveset is 
  block { 
    m[("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9);
  } with m
```

<!--Cameligo-->

We can update a big map in CameLIGO using the `Big_map.update` built-in:

```cameligo

let updated_map: moveset =
  Big_map.update ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) (Some (4,9)) moves
```

<!--Reasonligo-->

We can update a big map in ReasonLIGO using the `Big_map.update` built-in:

```reasonligo
let updated_map: moveset =
  Big_map.update(("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some((4,9)), moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Records

Records are a construct introduced in LIGO, and are not natively available in Michelson. The LIGO compiler translates records into Michelson `Pairs`.

Here's how a custom record type is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type user is record 
    id: nat;
    is_admin: bool;
    name: string;
end
```

<!--CameLIGO-->
```cameligo
type user = {
  id: nat;
  is_admin: bool;
  name: string;
}
```

<!--ReasonLIGO-->
```reasonligo
type user = {
  id: nat,
  is_admin: bool,
  name: string
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

And here's how a record value is populated:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const user: user = record
    id = 1n;
    is_admin = True;
    name = "Alice";
end
```

<!--CameLIGO-->
```cameligo
let user: user = {
  id = 1n;
  is_admin = true;
  name = "Alice";
}
```

<!--ReasonLIGO-->
```reasonligo
let user: user = {
  id: 1n, 
  is_admin: true, 
  name: "Alice"
};
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Accessing record keys by name

If we want to obtain a value from a record for a given key, we can do the following:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const is_admin: bool = user.is_admin;
```

<!--CameLIGO-->
```cameligo
let is_admin: bool = user.is_admin
```

<!--ReasonLIGO-->
```reasonligo
let is_admin: bool = user.is_admin;
```

<!--END_DOCUSAURUS_CODE_TABS-->
