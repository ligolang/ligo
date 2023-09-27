---
id: maps-records
title: Records/Objects and Maps
---

import Syntax from '@theme/Syntax';

So far, we have seen pretty basic data types. LIGO also offers more
complex built-in constructs, such as *records* and *maps*.

## Records/Objects

<Syntax syntax="cameligo">

Records are one-way data of different types can be packed into a
single type. A record is made of a set of *fields*, which are made of
a *field name* and a *field type*. Given a value of a record type, the
value bound to a field can be accessed by giving its field name to a
special operator (`.`).

Let us first consider an example of record type declaration.

```cameligo group=records1
type user = {
  id       : nat;
  is_admin : bool;
  name     : string
}
```

</Syntax>

<Syntax syntax="jsligo">

Objects are one-way data of different types can be packed into a
single type. An object is made of a set of *properties*, which are
made of a *property name* and a *property type*. Given a value of a
record type, the value bound to a field can be accessed by giving its
field name to a special operator (`.`).

Let us first consider an example of object type declaration.

```jsligo group=records1
type user = {
  id       : nat,
  is_admin : bool,
  name     : string
};
```

</Syntax>

<Syntax syntax="cameligo">

And here is how a record value is defined:

```cameligo group=records1
let alice : user = {
  id       = 1n;
  is_admin = true;
  name     = "Alice"
}
```

</Syntax>

<Syntax syntax="jsligo">

And here is how an object value is defined:

```jsligo group=records1
const alice : user = {
  id       : 1n,
  is_admin : true,
  name     : "Alice"
};
```

</Syntax>

### Accessing Record Fields

If we want the contents of a given field, we use the (`.`) infix
operator, like so:

<Syntax syntax="cameligo">

```cameligo group=records1
let alice_admin : bool = alice.is_admin
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=records1
const alice_admin = alice.is_admin;
```

</Syntax>

### Destructuring Records

We can also access fields of a record using the destructuring syntax.
This allows accessing multiple fields of a record in a concise manner, like so:

<Syntax syntax="cameligo">

```cameligo group=records1
let user_to_tuple (u : user) =
  let { id ; is_admin ; name } = u in
  (id, is_admin, name)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=records1
function userToTuple (u : user) {
  let { id, is_admin, name } = u;
  return [id, is_admin, name];
}
```

</Syntax>

We can ignore some fields of the records we can do so by
using `_` (underscore), like so:

<Syntax syntax="cameligo">

```cameligo group=records1
let get_id (u : user) =
  let { id ; is_admin = _ ; name = _ } = u in
  id
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=records1
function getId (u : user) {
  let { id, is_admin, name } = u;
  /* we don't use `is_admin` and `name`
   so prevent warning with `ignore` */
  ignore([is_admin, name]);
  return id
}
```

</Syntax>


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

<Syntax syntax="cameligo">

The syntax for the functional updates of record in CameLIGO follows
that of OCaml:

```cameligo group=records2
type point = {x : int; y : int; z : int}
type vector = {dx : int; dy : int}

let origin : point = {x = 0; y = 0; z = 0}

let xy_translate (p, vec : point * vector) : point =
  {p with x = p.x + vec.dx; y = p.y + vec.dy}
```

You can call the function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/maps-records/record_update.mligo
"({x=2;y=3;z=1}, {dx=3;dy=4})" --entry-point xy_translate
# Outputs: {z = 1 , y = 7 , x = 5}
```

> You have to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>

<Syntax syntax="jsligo">

The syntax for the functional updates of record in JsLIGO:

```jsligo group=records2
type point = {x: int, y: int, z: int}
type vector = {dx: int, dy: int}

const origin = {x: 0, y: 0, z: 0};

const xy_translate = (p: point, vec: vector) =>
  ({...p, x: p.x + vec.dx, y: p.y + vec.dy});
```

You can call the function `xy_translate` defined above by running the
following command of the shell:

```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/maps-records/record_update.jsligo \
  "xy_translate({x:2,y:3,z:1}, {dx:3,dy:4})"
# Outputs: record[x -> 5 , y -> 7 , z -> 1]
```

> It is important to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>

#### Nested updates

<Syntax syntax="cameligo">

A unique feature of LIGO is the ability to perform nested updates on
records. For example if you have the following record structure:

```cameligo
type color = Blue | Green

type preferences = {
  color : color;
  other : int
}

type account = {
  id          : int;
  preferences : preferences
}
```

</Syntax>

<Syntax syntax="jsligo">

A unique feature of LIGO is the ability to perform nested updates on records.
JsLIGO however does not support the specialised syntax as the other syntaxes.
The following however also does the trick.

For example if you have the following record structure:

```jsligo
type color = ["Blue"] | ["Green"];

type preferences = {
  color : color,
  other : int
};

type account = {
  id          : int,
  preferences : preferences
};
```

</Syntax>

You can update the nested record with the following code:

<Syntax syntax="cameligo">

```cameligo
let change_color_preference (account : account) (color : color) : account =
  { account with preferences.color = color }
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
const change_color_preference = (account : account, color : color) =>
  ({ ...account, preferences: {...account.preferences, color: color }});
```

</Syntax>

Note that all the records in the path will get updated. In this
example, those are `account` and `preferences`.

You can call the function `change_color_preference` defined above by running the
following command:

```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/maps-records/record_nested_update.jsligo \
  "change_color_preference({id:1001, preferences:{color:Blue(), other:1}}, Green())"
# Outputs: record[id -> 1001 , preferences -> record[color -> Green(unit) , other -> 1]]
```

### Comparison

<Syntax syntax="cameligo">

Record types are comparable, which allows to check for equality and
use records as key in sets or maps. By default, the ordering of
records is **undefined and implementation-dependent**. Ultimately, the
order is determined by the translated Michelson type. When using the
`@layout comb` (or `@layout:comb`) attribute, fields are translated in
their order in the record, and records are then ordered with
lexicographic ordering.

</Syntax>

<Syntax syntax="jsligo">

Record types are comparable, which allows to check for equality and
use records as key in sets or maps. By default, the ordering of
records is **undefined and implementation-dependent**. Ultimately, the
order is determined by the translated Michelson type. When using the
decorator `@layout("comb")`, fields are translated in their order in
the record, and objects are then ordered with lexicographic ordering.

</Syntax>

## Maps

*Maps* are a data structure which associate values of the same type to
values of the same type. The former are called *key* and the latter
*values*. Together they make up a *binding*. An additional requirement
is that the type of the keys must be *comparable*, in the Michelson
sense.

### Declaring a Map

Here is how a custom map from addresses to a pair of integers is
defined.

<Syntax syntax="cameligo">

```cameligo group=maps
type move = int * int
type register = (address, move) map
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
type move = [int, int];
type register = map<address, move>;
```

</Syntax>


### Creating an Empty Map

Here is how to create an empty map.

<Syntax syntax="cameligo">

```cameligo group=maps
let empty : register = Map.empty
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
const empty: register = Map.empty;
```

</Syntax>


### Creating a Non-empty Map

And here is how to create a non-empty map value:

<Syntax syntax="cameligo">

```cameligo group=maps
let moves : register =
  Map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

The `Map.literal` predefined function builds a map from a list of
key-value pair tuples, `(<key>, <value>)`.  Note also the `;` to
separate individual map entries.  `("<string value>": address)` means
that we type-cast a string into an address.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
const moves : register =
  Map.literal (list([
    ["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, [1,2]],
    ["tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, [0,3]]]));
```

The `Map.literal` predefined function builds a map from a list of
key-value pair tuples, `[<key>, <value>]`.  Note also the `,` to
separate individual map entries.  `"<string value>" as address` means
that we type-cast a string into an address.

</Syntax>


### Accessing Map Bindings

<Syntax syntax="cameligo">

```cameligo group=maps
let my_balance : move option =
  Map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
const my_balance: option<move> =
  Map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, moves);
```

</Syntax>


Notice how the value we read is an optional value: this is to force
the reader to account for a missing key in the map. This requires
*pattern matching*.

<Syntax syntax="cameligo">

```cameligo group=maps
let force_access (key, moves : address * register) : move =
  match Map.find_opt key moves with
    Some move -> move
  | None -> failwith "No move."
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
let force_access = (key: address, moves: register) => {
  return match(Map.find_opt (key, moves)) {
    when(Some(move)): move;
    when(None()): failwith("No move.")
  };
};
```

</Syntax>


### Updating a Map

Given a map, we may want to add a new binding, remove one, or modify
one by changing the value associated to an already existing key. All
those operations are called *updates*.

<Syntax syntax="cameligo">

We can update a binding in a map in CameLIGO by means of the
`Map.update` built-in function:

```cameligo group=maps
let assign (m : register) : register =
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) m
```

Notice the optional value `Some (4,9)` instead of `(4,9)`. If we had
use `None` instead, that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```cameligo group=maps
let add (m : register) : register =
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (4,9) m
```

</Syntax>

<Syntax syntax="jsligo">

We can update a binding in a map in JsLIGO by means of the
`Map.update` built-in function:

```jsligo group=maps
const assign = (m: register) =>
  Map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, Some ([4, 9]), m);
```

Notice the optional value `Some ([4,9])` instead of `[4, 9]`. If we used
`None` instead that would have meant that the binding is removed.

As a particular case, we can only add a key and its associated value.

```jsligo group=maps
const add = (m: register) =>
  Map.add
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, [4, 9], m);
```

</Syntax>

To remove a binding from a map, we need its key.

<Syntax syntax="cameligo">

In CameLIGO, we use the predefined function `Map.remove` as follows:

```cameligo group=maps
let delete (key, moves : address * register) : register =
  Map.remove key moves
```

</Syntax>

<Syntax syntax="jsligo">

In JsLIGO, we use the predefined function `Map.remove` as follows:

```jsligo group=maps
const delete = (key: address, moves: register) =>
  Map.remove(key, moves);
```

</Syntax>



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
useful if, for example you would like to check that each value inside
of a map is within a certain range and fail with an error otherwise.

The predefined functional iterator implementing the iterated operation
over maps is called `Map.iter`. In the following example, the register
of moves is iterated to check that the start of each move is above
`3`.

<Syntax syntax="cameligo">

```cameligo group=maps
let iter_op (m : register) : unit =
  let predicate = fun (i,j : address * move) -> assert (j.0 > 3)
  in Map.iter predicate m
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
const assert_all_greater_than_three = (m: register) => {
  let predicate = ([i, j]: [address, move]) => assert(j[0] > 3);
  Map.iter(predicate, m);
};
```

</Syntax>


#### Map Operations over Maps

We may want to change all the bindings of a map by applying to them a
function. This is called a *map operation*, not to be confused with
the map data structure. The predefined functional iterator
implementing the map operation over maps is called `Map.map`. In the
following example, we add `1` to the ordinate of the moves in the
register.

<Syntax syntax="cameligo">

```cameligo group=maps
let map_op (m : register) : register =
  let increment = fun (_,j : address * move) -> j.0, j.1 + 1
  in Map.map increment m
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
const map_op = (m: register) => {
  let increment = ([_a, j]: [address, move]) => [j[0], j[1] + 1];
  return Map.map(increment, m);
};
```

</Syntax>


#### Folded Operations over Maps

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.

The predefined functional iterator implementing the folded operation
over maps is called `Map.fold` and is used as follows.

<Syntax syntax="cameligo">

```cameligo group=maps
let fold_op (m : register) : int =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=maps
const fold_op = (m: register): int => {
  let folded = ([i, j]: [int, [address, move]]) => i + j[1][1];
  return Map.fold(folded, m, 5);
};
```

</Syntax>


## Big Maps

Ordinary maps are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a map holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big maps*. Big maps are a data structure offered by
Michelson which handles the scaling concerns for us. In LIGO, the
interface for big maps is analogous to the one used for ordinary maps.

### Declaring a Map

Here is how we define a big map:

<Syntax syntax="cameligo">

```cameligo group=big_maps
type move = int * int
type register = (address, move) big_map
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
type move = [int, int];
type register = big_map<address, move>;
```

</Syntax>


### Creating an Empty Big Map

Here is how to create an empty big map.

<Syntax syntax="cameligo">

```cameligo group=big_maps
let empty : register = Big_map.empty
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
const empty: register = Big_map.empty;
```

</Syntax>

### Creating a Non-empty Map

And here is how to create a non-empty map value:

<Syntax syntax="cameligo">

```cameligo group=big_maps
let moves : register =
  Big_map.literal [
    (("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address), (1,2));
    (("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address), (0,3))]
```

The predefined function `Big_map.literal` constructs a big map from a
list of key-value pairs `(<key>, <value>)`. Note also the semicolon
separating individual map entries.  The annotated value `("<string>
value>" : address)` means that we cast a string into an address.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
const moves : register =
  Big_map.literal (list([
    ["tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address, [1, 2]],
    ["tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, [0, 3]]]));
```

The predefined function `Big_map.literal` constructs a big map from a
list of key-value pairs `[<key>, <value>]`. Note also the semicolon
separating individual map entries.  The annotated value `("<string>
value>" as address)` means that we cast a string into an address.

</Syntax>


### Accessing Values

If we want to access a move from our `register` above, we can use the
postfix `[]` operator to read the associated `move` value. However,
the value we read is an optional value (in our case, of type `option
(move)`), to account for a missing key. Here is an example:

<Syntax syntax="cameligo">

```cameligo group=big_maps
let my_balance : move option =
  Big_map.find_opt ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) moves
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=big_maps
const my_balance: option<move> =
  Big_map.find_opt("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, moves);
```

</Syntax>


### Updating Big Maps


<Syntax syntax="cameligo">

We can update a big map in CameLIGO using the `Big_map.update`
built-in:

```cameligo group=big_maps
let updated_map : register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address) (Some (4,9)) moves
```

</Syntax>

<Syntax syntax="jsligo">

We can update a big map in JsLIGO using the `Big_map.update`
built-in:

```jsligo group=big_maps
const updated_map: register =
  Big_map.update
    ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, Some([4, 9]), moves);
```

</Syntax>


### Removing Bindings

Removing a binding in a map is done differently according to the LIGO
syntax.

<Syntax syntax="cameligo">

In CameLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```cameligo group=big_maps
let updated_map : register =
  Big_map.remove ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN": address) moves
```

</Syntax>

<Syntax syntax="jsligo">

In JsLIGO, the predefined function which removes a binding in a map
is called `Map.remove` and is used as follows:

```jsligo group=big_maps
const updated_map_: register =
  Big_map.remove("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" as address, moves);
```

</Syntax>

<!-- updated use of entry -->