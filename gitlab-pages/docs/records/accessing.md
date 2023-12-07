---
id: accessing
title: Accessing
---

import Syntax from '@theme/Syntax';

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

If we want to access a field, we use the selection operator "`.`",
like so:

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
