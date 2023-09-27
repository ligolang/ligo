---
id: include
title: Including Other Contracts
---

import Syntax from '@theme/Syntax';

Let us say that we have a contract that is getting a too large. If it
has a modular structure, you might find it useful to use the
`#include` statement to split the contract up over multiple files.

You take the code that you want to include and put it in a separate
file, for example the contract names `included`:

<Syntax syntax="cameligo">

```cameligo group=included
(* This is "included.mligo" *)
(* Demonstrate CameLIGO inclusion statements, see includer.mligo *)

let foo = 144
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=included
// This is "included.jsligo"
// Demonstrate JsLIGO inclusion statements, see includer.jsligo

export const foo = 144;
```

</Syntax>

And then you can include this code using the `#include` statement like
so:

<Syntax syntax="cameligo">

```cameligo
#include "gitlab-pages/docs/advanced/src/included.mligo"

let bar = foo
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
#include "gitlab-pages/docs/advanced/src/included.jsligo"

const bar = foo;
```

</Syntax>

<!-- updated use of entry -->