---
id: maps-records
title: Records and Maps
---

So far we have seen pretty basic data types. LIGO also offers more
complex built-in constructs, such as *records* and *maps*.

## Records

Records are one way data of different types can be packed into a
single type. A record is made of a set of *fields*, which are made of
a *field name* and a *field type*. Given a value of a record type, the
value bound to a field can be accessed by giving its field name to a
special operator (`.`).

Let us first consider and example of record type declaration.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=a
type user is
  record [
    id       : nat;
    is_admin : bool;
    name     : string
  ]
```

<!--CameLIGO-->
```cameligo group=a
type user = {
  id       : nat;
  is_admin : bool;
  name     : string
}
```

<!--ReasonLIGO-->
```reasonligo group=a
type user = {
  id       : nat,
  is_admin : bool,
  name     : string
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

And here is how a record value is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
const alice : user =
  record [
    id       = 1n;
    is_admin = True;
    name     = "Alice"
  ]
```

<!--CameLIGO-->
```cameligo group=a
let alice : user = {
  id       = 1n;
  is_admin = true;
  name     = "Alice"
}
```

<!--ReasonLIGO-->
```reasonligo group=a
let alice : user = {
  id       : 1n,
  is_admin : true,
  name     : "Alice"
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing Record Fields

If we want the contents of a given field, we use the (`.`) infix
operator, like so:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=a
const alice_admin : bool = alice.is_admin
```

<!--CameLIGO-->
```cameligo group=a
let alice_admin : bool = alice.is_admin
```

<!--ReasonLIGO-->
```reasonligo group=a
let alice_admin : bool = alice.is_admin;
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Functional Updates

Given a record value, it is a common design pattern to update only a
small number of its fields. Instead of copying the fields that are
unchanged, LIGO offers a way to only update the fields that are
modified.

One way to understand the update of record values is the *functional
update*. The idea is to have an *expression* whose value is the
updated record.

Let us consider defining a function that translates three-dimensional
points on a plane.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, the shape of that expression is `<record variable> with
<record value>`. The record variable is the record to update and the
record value is the update itself.

```pascaligo group=b
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

const origin : point = record [x = 0; y = 0; z = 0]

function xy_translate (var p : point; const vec : vector) : point is
  p with record [x = p.x + vec.dx; y = p.y + vec.dy]
```

You can call the function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/maps-records/record_update.ligo
translate "(record [x=2;y=3;z=1], record [dx=3;dy=4])"
# Outputs: {z = 1 , y = 7 , x = 5}
```

You have to understand that `p` has not been changed by the functional
update: a namless new version of it has been created and returned by
the blockless function.

<!--CameLIGO-->

The syntax for the functional updates of record in CameLIGO follows
that of OCaml:

```cameligo group=b
type point = {x : int; y : int; z : int}
type vector = {dx : int; dy : int}

let origin : point = {x = 0; y = 0; z = 0}

let xy_translate (p, vec : point * vector) : point =
  {p with x = p.x + vec.dx; y = p.y + vec.dy}
```

You can call the function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/maps-records/record_update.mligo
xy_translate "({x=2;y=3;z=1}, {dx=3;dy=4})"
# Outputs: {z = 1 , y = 7 , x = 5}
```

> You have to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

<!--ReasonLIGO-->

The syntax for the functional updates of record in ReasonLIGO follows
that of ReasonML:

```reasonligo group=b
type point = {x : int, y : int, z : int};
type vector = {dx : int, dy : int};

let origin : point = {x : 0, y : 0, z : 0};

let xy_translate = ((p, vec) : (point, vector)) : point =>
  {...p, x : p.x + vec.dx, y : p.y + vec.dy};
```
<!--END_DOCUSAURUS_CODE_TABS-->

You can call the function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/maps-records/record_update.religo
xy_translate "({x:2,y:3,z:1}, {dx:3,dy:4})"
# Outputs: {z = 1 , y = 7 , x = 5}
```

You have to understand that `p` has not been changed by the functional
update: a nameless new version of it has been created and returned.

### Record Patches

Another way to understand what it means to update a record value is to
make sure that any further reference to the value afterwards will
exhibit the modification. This is called a `patch` and this is only
possible in PascaLIGO, because a patch is an *instruction*, therefore
we can only use it in a block. Similarly to a *functional update*, a
patch takes a record to be updated and a record with a subset of the
fields to update, then applies the latter to the former (hence the
name "patch").

Let us consider defining a function that translates three-dimensional
points on a plane.

```pascaligo group=c
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

const origin : point = record [x = 0; y = 0; z = 0]

function xy_translate (var p : point; const vec : vector) : point is
  block {
    patch p with record [x = p.x + vec.dx];
    patch p with record [y = p.y + vec.dy]
  } with p
```

You can call the function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/maps-records/record_patch.ligo
xy_translate "(record [x=2;y=3;z=1], record [dx=3;dy=4])"
# Outputs: {z = 1 , y = 7 , x = 5}
```

Of course, we can actually translate the point with only one `patch`,
as the previous example was meant to show that, after the first patch,
the value of `p` indeed changed. So, a shorter version would be

```pascaligo group=d
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

const origin : point = record [x = 0; y = 0; z = 0]

function xy_translate (var p : point; const vec : vector) : point is
  block {
    patch p with record [x = p.x + vec.dx; y = p.y + vec.dy]
  } with p
```

You can call the new function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/maps-records/record_patch2.ligo
xy_translate "(record [x=2;y=3;z=1], record [dx=3;dy=4])"
# Outputs: {z = 1 , y = 7 , x = 5}
```

Record patches can actually be simulated with functional updates. All
we have to do is *declare a new record value with the same name as the
one we want to update* and use a functional update, like so:

```pascaligo group=e
type point is record [x : int; y : int; z : int]
type vector is record [dx : int; dy : int]

const origin : point = record [x = 0; y = 0; z = 0]

function xy_translate (var p : point; const vec : vector) : point is
  block {
    const p : point = p with record [x = p.x + vec.dx; y = p.y + vec.dy]
  } with p
```

You can call the new function `xy_translate` defined above by running the
following command of the shell:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/maps-records/record_simu.ligo
xy_translate "(record [x=2;y=3;z=1], record [dx=3;dy=4])"
# Outputs: {z = 1 , y = 7 , x = 5}
```

The hiding of a variable by another (here `p`) is called `shadowing`.

## Maps

*Maps* are a data structure which associate values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

Here is how a custom map from addresses to a pair of integers is
defined.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=f
type move is int * int
type register is map (address, move)
```

<!--CameLIGO-->
```cameligo group=f
type move = int * int
type register = (address, move) map
```

<!--ReasonLIGO-->
```reasonligo group=f
type move = (int, int);
type register = map (address, move);
```
<!--END_DOCUSAURUS_CODE_TABS-->

And here is how a map value is defined:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

```pascaligo group=f
const moves : register =
  map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

> Notice the `->` between the key and its value and `;` to separate
> individual map entries. The annotated value `("<string value>" :
> address)` means that we cast a string into an address. Also, `map`
> is a keyword.

<!--CameLIGO-->
```cameligo group=f
let moves : register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

> The `Map.literal` predefined function builds a map from a list of
> key-value pair tuples, `(<key>, <value>)`.  Note also the `;` to
> separate individual map entries.  `("<string value>": address)`
> means that we type-cast a string into an address.

<!--ReasonLIGO-->
```reasonligo group=f
let moves : register =
  Map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

> The `Map.literal` predefined function builds a map from a list of
> key-value pair tuples, `(<key>, <value>)`.  Note also the `;` to
> separate individual map entries.  `("<string value>": address)`
> means that we type-cast a string into an address.

<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing Map Bindings

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

In PascaLIGO, we can use the postfix `[]` operator to read the `move`
value associated to a given key (`address` here) in the register. Here
is an example:

```pascaligo group=f
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

<!--CameLIGO-->
```cameligo group=f
let my_balance : move option =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

<!--ReasonLIGO-->
```reasonligo group=f
let my_balance : option (move) =
  Map.find_opt (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=f
function force_access (const key : address; const moves : register) : move is
  case moves[key] of
    Some (move) -> move
  | None -> (failwith ("No move.") : move)
  end
```

<!--CameLIGO-->
```cameligo group=f
let force_access (key, moves : address * register) : move =
  match Map.find_opt key moves with
    Some move -> move
  | None -> (failwith "No move." : move)
```

<!--ReasonLIGO-->
```reasonligo group=f
let force_access = ((key, moves) : (address, register)) : move => {
  switch (Map.find_opt (key, moves)) {
  | Some (move) => move
  | None => failwith ("No move.") : move
  }
};
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Updating a Map

Given a map, we may want to add a new binding, remove one, or modify
one by changing the value associated to an already existing key. We
may even want to retain the key but not the associated value. All
those operations are called *updates*.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

The values of a PascaLIGO map can be updated using the usual
assignment syntax `<map variable>[<key>] := <new value>`. Let us
consider an example.

```pascaligo group=f
function assign (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m
```

If multiple bindings need to be updated, PascaLIGO offers a *patch
instruction* for maps, similar to that for records.

```pascaligo group=f
function assignments (var m : register) : register is
  block {
    patch m with map [
      ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (4,9);
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2)
    ]
  } with m
```

<!--CameLIGO-->

We can update a binding in a map in CameLIGO by means of the
`Map.update` built-in function:

```cameligo group=f
let assign (m : register) : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) m
```

> Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
> use `None` instead, that would have meant that the binding is
> removed.

<!--ReasonLIGO-->

We can update a binding in a map in ReasonLIGO by means of the
`Map.update` built-in function:

```reasonligo group=f
let assign = (m : register) : register =>
  Map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), Some ((4,9)), m);
```

> Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
> use `None` instead, that would have meant that the binding is
> removed.

<!--END_DOCUSAURUS_CODE_TABS-->

To remove a binding from a map, we need its key.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, there is a special instruction to remove a binding from
a map.
```pascaligo group=f
function delete (const key : address; var moves : register) : register is
  block {
    remove key from map moves
  } with moves
```

<!--CameLIGO-->

In CameLIGO, we use the predefined function `Map.remove` as follows:

```cameligo group=f
let delete (key, moves : address * register) : register =
  Map.remove key moves
```

<!--ReasonLIGO-->

In ReasonLIGO, we use the predefined function `Map.remove` as follows:

```reasonligo group=f
let delete = ((key, moves) : (address, register)) : register =>
  Map.remove (key, moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Functional Iteration over Maps

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO maps: the
*iterated operation*, the *map operation* (not to be confused with the
*map data structure*) and the *fold operation*.

#### Iterated Operation over Maps

The first, the *iterated operation*, is an iteration over the map with
no return value: its only use is to produce side-effects. This can be
useful if for example you would like to check that each value inside
of a map is within a certain range, and fail with an error otherwise.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, the predefined functional iterator implementing the
iterated operation over maps is called `map_iter`. In the following
example, the register of moves is iterated to check that the start of
each move is above `3`.

```pascaligo group=f
function iter_op (const m : register) : unit is
  block {
    function iterated (const i : address; const j : move) : unit is
      if j.1 > 3 then Unit else (failwith ("Below range.") : unit)
  } with map_iter (iterated, m)
```

> The iterated function must be pure, that is, it cannot mutate
> variables.

<!--CameLIGO-->

In CameLIGO, the predefinded functional iterator implementing the
iterated operation over maps is called `Map.iter`. In the following
example, the register of moves is iterated to check that the start of
each move is above `3`.

```cameligo group=f
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
```

<!--ReasonLIGO-->

In ReasonLIGO, the predefined functional iterator implementing the
iterated operation over maps is called `Map.iter`. In the following
example, the register of moves is iterated to check that the start of
each move is above `3`.

```reasonligo group=f
let iter_op = (m : register) : unit => {
  let predicate = ((i,j) : (address, move)) => assert (j[0] > 3);
  Map.iter (predicate, m);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### Map Operations over Maps

We may want to change all the bindings of a map by applying to them a
function. This is called a *map operation*, not to be confused with
the map data structure.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, the predefined functional iterator implementing the map
operation over maps is called `map_map` and is used as follows:

```pascaligo group=f
function map_op (const m : register) : register is
  block {
    function increment (const i : address; const j : move) : move is
      (j.1, j.2 + 1);
  } with map_map (increment, m)
```

> The mapped function must be pure, that is, it cannot mutate
> variables.

<!--CameLIGO-->

In CameLIGO, the predefined functional iterator implementing the map
operation over maps is called `Map.map` and is used as follows:

```cameligo group=f
let map_op (m : register) : register =
  let increment = fun (i,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
```

<!--ReasonLIGO-->

In ReasonLIGO, the predefined functional iteratir implementing the map
operation over maps is called `Map.map` and is used as follows:

```reasonligo group=f
let map_op = (m : register) : register => {
  let increment = ((i,j): (address, move)) => (j[0], j[1] + 1);
  Map.map (increment, m);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### Folded Operations over Maps

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, the predefined functional iterator implementing the
folded operation over maps is called `map_fold` and is used as
follows:

```pascaligo group=f
function fold_op (const m : register) : int is block {
  function folded (const j : int; const cur : address * move) : int is
    j + cur.2.2
  } with map_fold (folded, m, 5)
```

> The folded function must be pure, that is, it cannot mutate
> variables.

<!--CameLIGO-->

In CameLIGO, the predefined functional iterator implementing the
folded operation over maps is called `Map.fold` and is used as
follows:

```cameligo group=f
let fold_op (m : register) : register =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
```

<!--ReasonLIGO-->

In ReasonLIGO, the predefined functional iterator implementing the
folded operation over maps is called `Map.fold` and is used as
follows:

```reasonligo group=f
let fold_op = (m : register) : register => {
  let folded = ((i,j): (int, (address, move))) => i + j[1][1];
  Map.fold (folded, m, 5);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Big Maps

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary maps.

Here is how we define a big map:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=g
type move is int * int

type register is big_map (address, move)
```

<!--CameLIGO-->
```cameligo group=g
type move = int * int

type register = (address, move) big_map
```

<!--ReasonLIGO-->
```reasonligo group=g
type move = (int, int);

type register = big_map (address, move);
```
<!--END_DOCUSAURUS_CODE_TABS-->

And here is how a map value is created:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

```pascaligo group=g
const moves : register =
  big_map [
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) -> (1,2);
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) -> (0,3)]
```

> Notice the right arrow `->` between the key and its value and the
> semicolon separating individual map entries. The value annotation
> `("<string value>" : address)` means that we cast a string into an
> address.

<!--CameLIGO-->

```cameligo group=g
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

> The predefind function `Big_map.literal` constructs a big map from a
> list of key-value pairs `(<key>, <value>)`. Note also the semicolon
> separating individual map entries.  The annotated value `("<string
> value>" : address)` means that we cast a string into an address.

<!--ReasonLIGO-->

```reasonligo group=g
let moves : register =
  Big_map.literal ([
    ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address, (1,2)),
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, (0,3))]);
```

> The predefind function `Big_map.literal` constructs a big map from a
> list of key-value pairs `(<key>, <value>)`. Note also the semicolon
> separating individual map entries.  The annotated value `("<string
> value>" : address)` means that we cast a string into an address.


<!--END_DOCUSAURUS_CODE_TABS-->

### Accessing Values

If we want to access a move from our `register` above, we can use the
postfix `[]` operator to read the associated `move` value. However,
the value we read is an optional value (in our case, of type `option
(move)`), to account for a missing key. Here is an example:

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=g
const my_balance : option (move) =
  moves [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)]
```

<!--CameLIGO-->

```cameligo group=g
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

<!--ReasonLIGO-->

```reasonligo group=g
let my_balance : option (move) =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address, moves);
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Updating Big Maps

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

The values of a PascaLIGO big map can be updated using the
assignment syntax for ordinary maps

```pascaligo group=g
function add (var m : register) : register is
  block {
    m [("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address)] := (4,9)
  } with m

const updated_map : register = add (moves)
```

<!--CameLIGO-->

We can update a big map in CameLIGO using the `Big_map.update`
built-in:

```cameligo group=g
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

<!--ReasonLIGO-->

We can update a big map in ReasonLIGO using the `Big_map.update`
built-in:

```reasonligo group=g
let updated_map : register =
  Big_map.update
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), Some((4,9)), moves);
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Removing Bindings

Removing a binding in a map is done differently according to the LIGO
syntax.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

PascaLIGO features a special syntactic construct to remove bindings
from maps, of the form `remove <key> from map <map>`. For example,

```pascaligo group=g
function rem (var m : register) : register is
  block {
    remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) from map moves
  } with m

const updated_map : register = rem (moves)
```

<!--CameLIGO-->

In CameLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```cameligo group=g
let updated_map : register =
  Map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

<!--ReasonLIGO-->

In ReasonLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```reasonligo group=g
let updated_map : register =
  Map.remove (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address), moves)
```

<!--END_DOCUSAURUS_CODE_TABS-->
