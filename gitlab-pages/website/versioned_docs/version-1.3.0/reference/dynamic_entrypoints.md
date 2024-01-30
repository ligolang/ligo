---
id: dynamic-entrypoints-reference
title: Dynamic_entrypoints
description: Lazy entrypoints stored in the contract within a big_map. They can then be updated or removed without deploying a new contract.  
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

> Important: The `Test` module is only available inside the `ligo run test` command. See also [Testing LIGO](../advanced/testing.md).

<!--  set -->

<Syntax syntax="cameligo">

```cameligo skip
val set :
  ('p, 's) dynamic_entrypoint
  -> ('p, 's) entrypoint option 
  -> dynamic_entrypoints 
  -> dynamic_entrypoints
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const set : <P, S>(x1: dynamic_entrypoint<P, S>, x2: option<entrypoint>, x3: dynamic_entrypoints) => dynamic_entrypoints
```

</Syntax>

To set an dynamic entrypoint within a static entrypoint, just use `Dynamic_entrypoints.set`:

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let set_one (one_v2 : (unit, int) entrypoint) (s : storage) : operation list * storage =
  let dynamic_entrypoints =
    Dynamic_entrypoints.set one (Some one_v2) s.dynamic_entrypoints in
  [], {s with dynamic_entrypoints}
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@entry
const set_one = (one_v2 : entrypoint<unit, int>, s : storage) : [list<operation>, storage] => {
  let dynamic_entrypoints =
    Dynamic_entrypoints.set(one, Some(one_v2), s.dynamic_entrypoints);
  return [list([]), {...s, dynamic_entrypoints}]
}
```

</Syntax>

<!--  get -->

<Syntax syntax="cameligo">

```cameligo skip
val get : 
  ('p, 's) dynamic_entrypoint
  -> dynamic_entrypoints 
  -> ('p, 's) entrypoint option
  ```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const get : <P, S>(x1: dynamic_entrypoint<P, S>, x2: dynamic_entrypoints) => option<dynamic_entrypoint<P, S>>
```

</Syntax>


To get an dynamic entrypoint within a static entrypoint and call it just use `Dynamic_entrypoints.get`

<Syntax syntax="cameligo">

```cameligo skip
[@entry]
let call_one () (s : storage) : operation list * storage =
  match Dynamic_entrypoints.get one s.dynamic_entrypoints with
    Some f ->
      let op, storage = f () s.storage in
      op, {s with storage}
  | None -> failwith (-1)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
@entry
const call_one = ([], s : storage) : [list<operation>, storage] =>
  match (Dynamic_entrypoints.get(one, s.dynamic_entrypoints)) {
    when (Some(f)): do {
      const [op, storage] = f([], s.storage);
      return [op, ({...s, storage})]
    };
    when (None): failwith(-1);
  }
```

</Syntax>

<!--  set_bytes -->

<Syntax syntax="cameligo">

```cameligo skip
val set_bytes : 
  ('p, 's) dynamic_entrypoint
  -> bytes option
  -> dynamic_entrypoints 
  -> dynamic_entrypoints
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo skip
const set_bytes : <P, S>(x1: dynamic_entrypoint<P, S>, x2: option<bytes>, x3: dynamic_entrypoints) => dynamic_entrypoints
```

</Syntax>


You can use `Dynamic_entrypoints.set_entrypoint_bytes` to set an entrypoints to its bytes encoding directly. If your encoding is wrong, any call to `Dynamic_entrypoints.get` will fail at run-time

