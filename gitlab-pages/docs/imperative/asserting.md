---
id: asserting
title: Asserting
---

import Syntax from '@theme/Syntax';

Assertions can be used to ensure a certain condition is met when
running a contract. The predefined function `Assert.assert` is used to
check whether a given a Boolean condition holds. The function
`Assert.some` is used to check if an option value is not `None`. The
function `assert_some_with_error` is like `Assert.some` but an error
message can be given. Whenever the assertion fails, the contract will
stop and an error will be left on the execution stack.

<Syntax syntax="cameligo">

```cameligo group=asserting
let incr_if_true (b : bool) (n : int) : int =
  let () = Assert.assert b in n+1

let incr_if_some (b : unit option) (n : int) : int =
  let () = Assert.some b in n+1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=asserting
function incr_if_true (b: bool, n: int) : int {
  Assert.assert(b);
  return n+1;
};

function incr_if_some (b: option<unit>, n: int) : int {
  Assert.some(b);
  return n+1;
};
```

</Syntax>

You can use `Assert.Error.assert` or `Assert.Error.some` to use a
custom error message.

<Syntax syntax="cameligo">

```cameligo group=assert_with_error
let incr_if_true (b : bool) (n : int) : int =
  let () = Assert.Error.assert b "My custom error message."
  in n+1
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=assert_with_error
const incr_if_true = (b: bool, n: int) : int => {
  Assert.Error.assert (b, "My custom error message.");
  return n+1;
};
```

</Syntax>

Likewise, we can check for `None` instead of `Some` by using
`Assert.none` and `Assert.Error.none`.
