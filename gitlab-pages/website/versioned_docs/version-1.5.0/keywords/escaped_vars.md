---
id: escaped_vars
title: Escaped variables
---

import Syntax from '@theme/Syntax';

Keywords cannot be used as variables or record fields. If you need to
use a keyword as a variable, you can prefix it with `@`, like so:

<Syntax syntax="cameligo">

```cameligo group=keywords
let @from = ("tz1fakefakefakefakefakefakefakcphLA5" : address)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=keywords
const @from = ("tz1fakefakefakefakefakefakefakcphLA5" as address)
```


Note that this convention, called *escaped identifiers*, conflicts
with that of *decorators*, as found in JavaScript. Therefore,
decorators are not considered valid escaped identifiers, e.g. `@entry`
is invalid as a variable.

</Syntax>
