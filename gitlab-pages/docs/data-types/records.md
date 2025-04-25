---
title: Records
jsligoTitle: Objects
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Records are a structured data type that can include one or more fields, each with a name and a type.
A record type can contain any number of different data types, as in this example:

```cameligo group=records1
type user = {
  id       : nat;
  is_admin : bool;
  name     : string
}
```

</Syntax>

<Syntax syntax="jsligo">

As in JavaScript, objects (called *records* in previous versions of JsLIGO), are a structured data type that can include one or more fields, each with a name and a type.
A record type can contain any number of different data types, as in this example:

```jsligo group=objects1
type user = {
  id       : nat,
  is_admin : bool,
  name     : string
};
```

</Syntax>

<Syntax syntax="cameligo">

To create a variable of a record type, specify the name and value of each field, as in this example:

```cameligo group=records1
let alice : user = {
  id       = 1n;
  is_admin = true;
  name     = "Alice"
}
```

</Syntax>

<Syntax syntax="jsligo">

To create a variable of an object type, specify the name and value of each field, as in this example:

```jsligo group=objects1
const alice : user = {
  id       : 1 as nat,
  is_admin : true,
  name     : "Alice"
};
```

You can also use a semicolon (`;`) to separate the fields in a record instead of a comma.

</Syntax>

## Accessing

To get the value of a field, use a period (`.`) as the selection operator, as in this example:

<Syntax syntax="cameligo">

```cameligo group=record_access
type user = {
  login : string;
  name  : string
}

type account = {
  user     : user;
  id       : int;
  is_admin : bool
}

let user : user = {login = "al"; name = "Alice"}
let alice : account = {user; id=5; is_admin = true}
let is_alice_admin : bool = alice.is_admin // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=object_access
type user = {
  login : string,
  name  : string
};

type account = {
  user     : user,
  id       : int,
  is_admin : bool
};

const user: user = {login: "al", name: "Alice"};
const alice: account = {user, id: 5, is_admin: true};
const is_alice_admin = alice.is_admin; // == true
const alice_name = alice.user.name; // == "Alice"
```

Instead of using the field name and the selection operator, you can put the name of the field in square brackets, as in this example:

```jsligo group=object_access
const alice_id = alice["id"]; // 5
```

Unlike in JavaScript, you cannot use the name of a variable in square brackets to specify which field to retrieve.

</Syntax>

<Syntax syntax="cameligo">

You can also access fields of a record using a destructuring syntax,
known as _pattern matching_, which enables accessing multiple fields of
a record in parallel, like so:

```cameligo group=record_access
let user_to_triple (a : account) =
  let {user; id; is_admin} = a
  in user, id, is_admin
```

</Syntax>

<Syntax syntax="jsligo">

You can also access fields of a record using the destructuring
syntax, known as _pattern matching_, which allows accessing multiple
fields of a record in parallel, like so:

```jsligo group=object_access
function userToTuple (a: account) {
  const {user, id, is_admin} = a;
  return [user, id, is_admin];
}
```

</Syntax>

<Syntax syntax="cameligo">

If you do not use one or more of the fields in the record, assign them the special variable name `_` to avoid triggering a warning about an unused variable, like so:

```cameligo group=record_access
let get_id (a : account) =
  let {user=_; id; is_admin=_} = a // To avoid a warning
  in id
```

</Syntax>

<Syntax syntax="jsligo">

If you do not use one or more of the fields in the object, use the predefined function `ignore` on them to avoid triggering a warning about an unused variable, like so:

```jsligo group=object_access
function getId (a: account) {
  let {user, id, is_admin} = a;
  ignore([user, is_admin]); // To avoid a warning
  return id;
}
```

</Syntax>

## Assigning

<Syntax syntax="cameligo">

Given a record, it is a common design pattern to update only a small
number of its fields. Instead of forcing the programmer to copy the
remaining, unchanged fields, CameLIGO offers a way to update only the
fields that are modified.

One way to understand the update of records is the *functional
update*. The idea is to have an *expression* whose value is the
updated record.

Let us consider defining a function that translates three-dimensional
points on a plane.

The syntax for the functional updates of record in CameLIGO follows
that of OCaml:

```cameligo group=record_update
type point = {x : int; y : int; z : int}
type vector = {dx : int; dy : int}

let origin : point = {x = 0; y = 0; z = 0}

let xy_translate (p, vec : point * vector) : point =
  {p with x = p.x + vec.dx; y = p.y + vec.dy}
```

> It is important to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>

<Syntax syntax="jsligo">

You can change the fields of an object that is declared as a variable, as in this example:

```jsligo group=object_update
let my_object = {a: 1, b: 2};
my_object.a = 5;
my_object["b"] = 3;
```

:::note

Unlike in JavaScript, you cannot change the fields of an object that is declared as a constant.

:::

Similarly, you cannot add fields to an object after you create it, regardless of whether it is declared as a constant or a variable.
To add fields, you can use a *functional update* to create a new object that has all of the fields of one or more other objects with the updates that you want.
As in JavaScript, this type of update uses the `...` operator, as in this example:

```jsligo group=object_update
type point = {x: int, y: int, z: int}
type vector = {dx: int, dy: int}

const origin: point = {x: 0, y: 0, z: 0};

const xy_translate = (p: point, vec: vector) =>
  ({...p, x: p.x + vec.dx, y: p.y + vec.dy});
```

:::note

In the previous example, the constant `p` has not been changed by the functional update; a nameless new version of it has been created and returned.

:::

</Syntax>

<Syntax syntax="cameligo">

### Nested updates

A unique feature of CameLIGO is the ability to perform nested updates
on records. For example, given the following record declarations:

```cameligo group=record_nested_update
type user = {
  login : string;
  name  : string
}

type account = {
  user     : user;
  id       : int;
  is_admin : bool
}
```

You can update the record `user` nested inside `account` with the
following code:

```cameligo group=record_nested_update
let change_login (login : string) (account : account) : account =
  {account with user.login = login}
```

> Note: This is not possible in OCaml.

</Syntax>

## Comparing

<Syntax syntax="cameligo">

Record types are comparable types, which means that their values can
be implicitly compared for equality. As a result, records can be used
as keys in [sets](./sets.md) and [maps](./maps.md). By default, the
implicit, total order over records is **undefined and
implementation-dependent** — ultimately, the order is determined by
the translated Michelson type.

When using the `[@layout "comb"]` (or `[@layout:comb]`) attribute,
fields are translated in Michelson with their order as written in the
source code, and records are then ordered lexicographically (that is,
when two fields of the same name have the same values, another field
is compared, like ordering two English words according to the alphabet).

</Syntax>

<Syntax syntax="jsligo">

Object types are comparable types, which means that their values can
be implicitly compared for equality. As a result, objects can be used
as keys in [sets](./sets.md) and [maps](./maps.md). By default, the
implicit, total order over objects is **undefined and
implementation-dependent** — ultimately, the order is determined by
the translated Michelson type.

When using the `@layout("comb")` decorator, fields are translated in
Michelson with their order as written in the source code, and objects
are then ordered lexicographically (that is, when two fields of the
same name have the same values, another field is compared,
like ordering two English words according to the alphabet).

</Syntax>
