---
id: include
title: Including Other Contracts
---

Let us say that we have a contract that is getting a too large. If it
has a modular structure, you might find it useful to use the
`#include` statement to split the contract up over multiple files.

You take the code that you want to include and put it in a separate
file, for example `included.ligo`:

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo

// Demonstrate PascaLIGO inclusion statements, see includer.ligo

const foo : int = 144
```

<!--CameLIGO-->
```cameligo
// Demonstrate CameLIGO inclusion statements, see includer.mligo

let foo : int = 144
```

<!--ReasonLIGO-->
```reasonligo
// Demonstrate ReasonLIGO inclusion statements, see includer.religo

let foo : int = 144;
```

<!--END_DOCUSAURUS_CODE_TABS-->


And then you can include this code using the `#include` statement like so:

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
#include "included.ligo"

const bar : int = foo
```

<!--CameLIGO-->
```cameligo
#include "included.mligo"

let bar : int = foo
```

<!--ReasonLIGO-->
```reasonligo
#include "included.religo"

let bar : int = foo;
```

<!--END_DOCUSAURUS_CODE_TABS-->
