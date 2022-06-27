---
id: catch-error-view
title: How to catch an error thrown in a view ?
---

import Syntax from '@theme/Syntax';

Error thrown by calling `failwith` cannot be caught, the call is compiled to Michelson's `FAILWITH` instruction.

Therefore, if the error does not need to carry information, you can simply use the type `option` where `None` means that an error occurred.
If you need more information on the kind of error, you can either add variants to the correct values if their type is a sum type,
or define your own type result like in OCaml:

<Syntax syntax="pascaligo">

In PascaLIGO :

```pascaligo
type result (s,f) is Ok of s | Error of f
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO :

```cameligo
type ('s,'f) result = Ok of 's | Error of 'f
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO :

```reasonligo
type result ('s,'f) = Ok('s) | Error('f)
```

</Syntax>
<Syntax syntax="jsligo">

In JsLIGO :

```jsligo
type result<s,f> = ["Ok", s] | ["Error", f]
```

</Syntax>