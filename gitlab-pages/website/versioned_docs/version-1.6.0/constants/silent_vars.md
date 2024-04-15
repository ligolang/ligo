---
id: silent_vars
title: Silent variables
---

import Syntax from '@theme/Syntax';

When debugging LIGO programs, we might want to ignore a constant after
its definition. The LIGO compiler will however likely emit a warning
about an unused variable. The solution is to prefix the unused
variable by an underscore `_`, like so:

<Syntax syntax="cameligo">

```cameligo group=silent_variables
let f _x = 0 // _x is silent
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=silent_variables
function f (_x) { return 0; }; // _x is silent
```

</Syntax>

<Syntax syntax="jsligo">

Note that the variable `_` is a special case of silent variable:

```jsligo group=silent_variables
const _ = 1;
const a = _;
```

</Syntax>
