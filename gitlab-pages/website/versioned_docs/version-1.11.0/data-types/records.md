---
title: Records
---

import Syntax from '@theme/Syntax';

So far, we have seen relatively simple data types. LIGO also offers
more complex built-in constructs, such as *records*.

Records are one-way data of different types can be packed into a
single type. A record is made of a set of *fields*, which are made of
a *field name* and a *field type*. Given a record, the value bound to
a field is accessed by giving its name to the selection operator
"`.`".

Let us first consider an example of record type declaration.

<Syntax syntax="cameligo">

```cameligo group=records1
type user = {
  id       : nat;
  is_admin : bool;
  name     : string
}
```

</Syntax>

<Syntax syntax="jsligo">

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

And here is how a record value is defined:

```jsligo group=records1
const alice : user = {
  id       : 1n,
  is_admin : true,
  name     : "Alice"
};
```

> Note: A semicolon `;` can also separate fields instead of a
> comma.

</Syntax>

## Accessing

<Syntax syntax="cameligo">

If we want the contents of a given field, we use the selection operator
"`.`", like so:

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

If we want to access a field, we use the selection operator "`.`"
followed by the field name, like so:

```jsligo group=record_access
type user = {
  login : string,
  name  : string
};

type account = {
  user     : user,
  id       : int,
  is_admin : bool
};

const user : user = {login: "al", name: "Alice"};
const alice : account = {user, id: 5, is_admin: true};
const is_alice_admin = alice.is_admin; // == true
```

Instead of the field name, we can provide between square brackets a
string that contains the field name, or an integer that is the index
of the field in the record declaration:

</Syntax>

<Syntax syntax="cameligo">

We can also access fields of a record using a destructuring syntax,
known as _pattern matching_, which enables accessing multiple fields of
a record in parallel, like so:

```cameligo group=record_access
let user_to_triple (a : account) =
  let {user; id; is_admin} = a
  in user, id, is_admin
```

</Syntax>

<Syntax syntax="jsligo">

We can also access fields of a record using the destructuring
syntax, known as _pattern matching_, which allows accessing multiple
fields of a record in parallel, like so:

```jsligo group=record_access
function userToTuple (a : account) {
  const {user, id, is_admin} = a;
  return [user, id, is_admin];
}
```

</Syntax>

<Syntax syntax="cameligo">

If we do not use some of the fields we matched, we assign them the
special variable `_`, to avoid triggering a warning about an unused
variable, like so:

```cameligo group=record_access
let get_id (a : account) =
  let {user=_; id; is_admin=_} = a // To avoid a warning
  in id
```

</Syntax>

<Syntax syntax="jsligo">

We can ignore some fields by calling the predefined function
`ignore` on them, like so:

```jsligo group=record_access
function getId (a : account) {
  let {user, id, is_admin} = a;
  ignore([user, is_admin]); // To avoid a warning
  return id;
}
```

</Syntax>

## Assigning

Given a record, it is a common design pattern to update only a small
number of its fields. Instead of forcing the programmer to copy the
remaining, unchanged fields, CameLIGO offers a way to only update the
fields that are modified.

One way to understand the update of records is the *functional
update*. The idea is to have an *expression* whose value is the
updated record.

Let us consider defining a function that translates three-dimensional
points on a plane.

<Syntax syntax="cameligo">

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

The syntax for the functional updates is:

```jsligo group=record_update
type point = {x: int, y: int, z: int}
type vector = {dx: int, dy: int}

const origin = {x: 0, y: 0, z: 0};

const xy_translate = (p: point, vec: vector) =>
  ({...p, x: p.x + vec.dx, y: p.y + vec.dy});
```

> It is important to understand that `p` has not been changed by the
> functional update: a nameless new version of it has been created and
> returned.

</Syntax>

### Nested updates

<Syntax syntax="cameligo">

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


<Syntax syntax="jsligo">

JsLIGO does not support functional updates of nested records. For
example, if you have the following record declarations:

```jsligo group=record_nested_update
type user = {
  login : string,
  name  : string
};

type account = {
  user     : user,
  id       : int,
  is_admin : bool
};
```

You can update the record `user` nested inside `account` the long way:

```jsligo group=record_nested_update
const change_login = (login: string, account: account) : account =>
  ({...account, user: {...account.user, login}});
```

</Syntax>

## Comparing

Record types are comparable types, which means that their values can
be implicitly compared for equality, and records can be used as keys
in [sets](./sets.md) or [maps](./maps.md). By
default, the implicit, total order over records is **undefined and
implementation-dependent** --- ultimately, the order is determined by
the translated Michelson type.

<Syntax syntax="cameligo">

When using the `[@layout "comb"]` (or `[@layout:comb]`) attribute,
fields are translated in Michelsom with their order as written in the
source code, and records are then ordered lexicographically (that is,
when two fields of the same name have the same values, another field
is compared, much rather like ordering two English words according to
the alphabet).

</Syntax>

<Syntax syntax="jsligo">

When using the `@layout("comb")` decorator, fields are translated in
Michelsom with their order as written in the source code, and records
are then ordered lexicographically (that is, when two fields of the
same name have the same values, another field is compared, much rather
like ordering two English words according to the alphabet).

</Syntax>
