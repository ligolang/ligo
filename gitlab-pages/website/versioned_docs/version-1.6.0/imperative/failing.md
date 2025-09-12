---
id: failing
title: Failing
---

import Syntax from '@theme/Syntax';

In some cases it is convenient to interrupt the flow of execution with
a failure instead of a value: this is where the predefined function
`failwith` comes in.

The failwith function raises an error that cannot be caught and
terminates the execution of the contract.

<Syntax syntax="cameligo">

```cameligo group=failwith
let check _param : unit =
  failwith "This function always fails."
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=failwith
const check =
  (_param) : unit => failwith("This function always fails");
```

</Syntax>

The call to `failwith` sometimes needs to be annotated with a type
when the type-checker cannot infer the correct type, e.g. `(failwith
"message" : type_of_result)`.
