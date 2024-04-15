---
id: entry
title: entry
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@entry]` on a function marks said function as an
*entrypoint* of the contract, either at top-level or in a module that
is then used as a contract. Here is an example of entrypoints defined
in a module:

```cameligo group=entry
type storage = int
type return = operation list * storage

module Foo = struct
  [@entry]
  let decrement (param : int) (storage : storage) : return =
    [], storage - param

  [@entry]
  let increment (param : int) (storage : storage) : return =
    [], storage + param

  [@entry]
  let reset () (_ : storage) : return = [], 0

  [@view]
  let get_storage () (storage : storage) : storage = storage
end
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@entry` on a function marks said function as an
*entrypoint* of the contract, either at top-level or in a namespace
that is then used as a contract.

```jsligo group=entry
type storage = int;
type @return = [list<operation>, storage];

namespace Foo {
  @entry
  const decrement = (param: int, storage: storage) : @return =>
    [[], storage - param];

  @entry
  const increment = (param: int, storage: storage) : @return =>
    [[], storage + param];

  @entry
  const reset = (_u: unit, _s: storage) : @return =>
    [[], 0];

  @view
  const get_storage = (_: unit, storage: storage) : storage => storage;
};
```

Note that if you want to define an entrypoint called `default`, use
the escaped identifier `@default` because `default` is a keyword.

</Syntax>
