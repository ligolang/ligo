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
type 'storage return = operation list * 'storage

[@view]
let add (param : int) (storage : int) : int = param + storage

[@view]
let get_storage (_ : int) (storage : int) : int = storage

[@entry]
let main () (storage : int) : int return =
  ([] : operation list), storage

```

</Syntax>

<Syntax syntax="jsligo">

The decorator is `@view` and placed just before a function, like the
`@entry` decorator, for example.

</Syntax>
