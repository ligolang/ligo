---
id: decorators
title: Attributes
jsligoTitle: Decorators
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

It is sometimes useful or necessary to modify the default semantics of
a LIGO construct. This is done by annotating the construct with an
*attribute*. Attributes have one of the three forms:

  1. `[@name_of_attribute]`,
  2. `[@name_of_attribute "Some string"]`,
  3. `[@name_of_attribute name]`.

Attributes are located just before the construct they apply to,
contrary to OCaml. (Also, contrary to OCaml, LIGO attributes only have
one
`[@` opening syntax.) Attributes can be composed by simple juxtaposition, like so:

```cameligo group=attributes
type storage = int
type result = operation list * storage

[@entry] [@no_mutation]
let sub (delta : int) (store : storage) : result =
  [], store - delta
```

## Internal attributes

Furthermore, the following attributes are used internally by the
compiler. You may encounter them when exporting the Abstract Syntax
Tree (AST) after a certain compilation pass, but they should not
appear in normal source code:

* `[@thunk]`
* `[@hidden]`
* `[@public]`

</Syntax>

<Syntax syntax="jsligo">

It is sometimes useful or necessary to modify the default semantics of
a LIGO construct. This is done by annotating the construct with a
*decorator*. Decorators have one of the two forms:

  1. `@name_of_decorator`,
  2. `@name_of_decorator("Some string")`.

Decorators are located just before the construct they apply to. Note
that the lexical convention for decorators clashes with that of
[escaped variables](../../keywords/escaped_vars.md), therefore
*predefined decorators cannot be variables.*

Decorators can be composed by simple juxtaposition, like so:

```jsligo group=decorators
type storage = int;
type result = [list<operation>, storage];

@entry @no_mutation
const sub = (delta: int, store: storage) : result =>
  [[], store - delta];
```

</Syntax>
