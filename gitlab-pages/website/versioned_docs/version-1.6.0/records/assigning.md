---
id: assigning
title: Assigning
---

import Syntax from '@theme/Syntax';

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

## Nested updates

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

## Comparison

Record types are comparable types, which means that their values can
be implicitly compared for equality, and records can be used as keys
in [sets](../sets/declaring.md) or [maps](../maps/declaring.md). By
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
