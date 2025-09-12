---
id: declaring
title: Declaring
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
