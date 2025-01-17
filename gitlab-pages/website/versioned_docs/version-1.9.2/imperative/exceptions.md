---
id: exceptions
title: Exceptions
---

import Syntax from '@theme/Syntax';

On Tezos, exceptions cannot be caught; there is no way to intercept exceptions and trigger alternate behavior.
If an exception occurs, **the entire transaction is backtracked**.
In this case, all of the operations are canceled and any completed operations are reverted, as if the initial call to the contract never happened.

:::info

A transaction starts with a call from a user account (sometimes known as an _implicit account_) and includes all subsequent operations, such as tez transfers, contract calls, and contract originations that the initial call triggered.

For example, if a user account calls contract A, contract A calls contract B, contract B calls contract C, and contract C raises an exception, all calls are reverted, including the original call to contract A.
The storage states of all of these contracts and the balances of all related accounts are set back to their state before the original call, as if that call never happened.

:::

To raise an error, abort the current transaction, and abort all transactions that led to the current transaction, use the `failwith` function.

<Syntax syntax="cameligo">

```cameligo group=failwith
type storage = unit
type result = operation list * storage

[@entry]
let main (_param : unit) (_store : storage) : result =
  failwith "This contract always fails."
```

The call to the `failwith` function sometimes needs to be annotated with a type when the type-checker cannot infer the correct type, as in `(failwith "message" : result)`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=failwith
@entry
const main = (p: unit, s: unit) : [list<operation>, unit] =>
  failwith("This contract always fails");
```

The call to the `failwith` function sometimes needs to be annotated with a type when the type-checker cannot infer the correct type, as in `return (failwith("message") : result);`.

</Syntax>

<Syntax syntax="jsligo">

JsLIGO does not support the JavaScript `try/catch` framework for catching exceptions.

</Syntax>

Failed assertions cause exceptions; see [Asserting](./asserting).

<!-- updated use of entry -->
