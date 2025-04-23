---
id: decorators
title: Attributes
jsligoTitle: Decorators
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

Attributes modify the default semantics of a piece of code.
Attributes come in three forms:

1. `[@name_of_attribute]`
2. `[@name_of_attribute "Some string"]`
3. `[@name_of_attribute name]`

Attributes are located just before the construct they apply to, contrary to OCaml.
Also contrary to OCaml, LIGO attributes always use only one `@` symbol.

The most common use of an attribute is to denote an [entrypoint](../syntax/contracts/entrypoints).

Attributes are placed immediately before the code they apply to.
You can also apply multiple attributes on one line, as in this example:

```cameligo group=attributes
type storage = int
type result = operation list * storage

[@entry] [@no_mutation]
let sub (delta : int) (storage : storage) : result =
  [], storage - delta
```

## Internal attributes

The following attributes are used internally by the compiler.
You may encounter them when exporting the Abstract Syntax Tree (AST) after a certain compilation pass, but they should not appear in normal source code:

* `[@thunk]`
* `[@hidden]`

## List of attributes

LIGO supports these attributes:

</Syntax>

<Syntax syntax="jsligo">

Decorators modify the default semantics of a piece of code.
Decorators come in two forms:

1. `@name_of_decorator`
2. `@name_of_decorator("Some string")`

The most common use of a decorator is to denote an [entrypoint](../syntax/contracts/entrypoints).

Decorators are placed immediately before the code they apply to.
You can also apply multiple decorators to the same piece of code, as in this example:

```jsligo group=decorators
type storage = int;
type result = [list<operation>, storage];

// @entry
// @no_mutation
const sub = (delta: int, storage: storage) : result =>
  [[], storage - delta];
```

Decorators must be in comments unless they are within a class.
For example, this class has decorators that are not in comments:

```jsligo group=decorators
class Counter {

  @entry
  @inline
  add = (value: int, storage: storage): result =>
    [[], storage + value];

  @entry
  @deprecated
  sub = (value: int, storage: storage): result =>
    [[], storage - value];

  @view
  get = (_: unit, storage: storage): storage => storage;
}
```

## List of decorators

LIGO supports these decorators:

</Syntax>

- [`annot`](../reference/decorators/annot)
- [`deprecated`](../reference/decorators/deprecated)
- [`dyn_entry`](../reference/decorators/dyn_entry)
- [`entry`](../reference/decorators/entry)
- [`inline`](../reference/decorators/inline)
- [`layout`](../reference/decorators/layout)
- [`private`](../reference/decorators/private)
- [`public`](../reference/decorators/public)
- [`view`](../reference/decorators/view)
