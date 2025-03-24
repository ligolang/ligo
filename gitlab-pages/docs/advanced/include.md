---
id: include
title: Including Other Contracts
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

Let us say that we have a contract that is getting a too large. If it
has a modular structure, you might find it useful to use the
`#include` statement to split the contract up over multiple files.

You take the code that you want to include and put it in a separate
file, for example the contract names `included`:

```cameligo group=included
(* This is "included.mligo" *)
(* Demonstrate CameLIGO inclusion statements, see includer.mligo *)

let foo = 144
```

And then you can include this code using the `#include` statement like
so:

```cameligo
#include "gitlab-pages/docs/advanced/src/include/included.mligo"

let bar = foo
```

</Syntax>
