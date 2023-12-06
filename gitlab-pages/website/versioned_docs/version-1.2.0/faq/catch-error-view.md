---
id: catch-error-view
title: How to catch an error thrown in a view ?
---

import Syntax from '@theme/Syntax';

Error thrown by calling `failwith` cannot be caught, the call is compiled to Michelson's `FAILWITH` instruction.

Therefore, if the error does not need to carry information, you can
simply use the type `option` where `None` means that an error
occurred.  If you need more information on the kind of error, you can
either add variants to the correct values if their type is a sum type,
or define your own type result like in OCaml:

<Syntax syntax="cameligo">

In CameLIGO:

```cameligo group=failwith_view
type ('success,'failure) result =
| Ok of 'success
| Error of 'failure
```

</Syntax>

<Syntax syntax="jsligo">

In JsLIGO:

```jsligo group=failwith_view
type result<success,failure> =
| ["Ok", success]
| ["Error", failure]
```

</Syntax>

An exception (pun not intended) to this is that when using the test framework, some functions are able to detect a `failwith` raised by one of their callbacks, and return a different result based on the success or failure.

For example, the [Test.transfer](../reference/test.md) function catches errors raised with `failwith` and converts them to a result of type `type test_exec_result = Success of nat | Fail of test_exec_error`.
This allows the programmer to write positive and negative test, checking that the contract runs as intended and fails as intended.

<!-- updated use of entry -->