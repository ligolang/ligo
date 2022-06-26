---
id: catch-error-view
title: How to catch an error thrown in a view ?
---

import Syntax from '@theme/Syntax';

Error thown by calling `failwith` cannot be caught, the call is compiled to Michelson's `FAIL` instruction.

Therefore, if the error does not need to carry information, you can simply use the type `option` where `None` means that an error occurred.
If you need more information on the kind of error, you can either add variants to the correct values if their type is a sum type,
or define your own type result like on OCaml:

In CameLigo :

```cameligo
type ('a,'b) result = Ok of 'a | Error of 'b
```
