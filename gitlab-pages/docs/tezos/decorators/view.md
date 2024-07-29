---
id: view
title: view
---

import Syntax from '@theme/Syntax'

*Views* are a form of read-only entrypoints that can be called
synchronously, and often are used to read part of the storage.

<Syntax syntax="cameligo">

The attribute is `[@view]` and placed just before a function, like the
`[@entry]` attribute, for example.

```cameligo group=view
type storage = int
type return_type = operation list * storage

[@view]
let add (param : int) (storage : storage) : int = param + storage

[@view]
let get_storage (_ : int) (storage : storage) : int = storage

[@entry]
let main () (storage : storage) : return_type =
  ([] : operation list), storage
```

</Syntax>

<Syntax syntax="jsligo">

The decorator is `@view` and placed just before a function, like the
`@entry` decorator, for example.

```jsligo group=view
type return_type = [list<operation>, int];

@view
const add = (param: int, storage: int): int => param + storage

@view
const get_storage = (_ : unit, storage: int): int => storage

@entry
const main = (_ : unit, storage: int): return_type =>
  [[], storage]
```

</Syntax>

For more information about views, see [Views](../../contract/views).
