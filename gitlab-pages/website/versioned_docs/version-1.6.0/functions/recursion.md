---
id: recursion
title: Recursion
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

As in OCaml, recursive functions are defined using the `let rec`
keywords:

```cameligo group=recursion
let rec sum (n, acc : int * int) : int =
  if n < 1 then acc else sum (n-1, acc + n)

let rec fibonacci (n, n_1, n_0 : int * int * int) : int =
  if n < 2 then n_1 else fibonacci (n-1, n_1 + n_0, n_1)
```
</Syntax>

<Syntax syntax="jsligo">

Recursive functions are defined and called using the same syntax as
non-recursive functions.

```jsligo group=recursion
function sum (n: int, acc: int) : int {
  if (n < 1) return acc else return sum (n-1, acc + n);
};

function fibonacci (n: int, n_1: int, n_0: int): int {
  if (n < 2) return n_1 else return fibonacci (n-1, n_1 + n_0, n_1);
};
```

This means that all values, including functions, declared in the same
block or top-level scope, must have different names, because they can
all potentially be mutually recursive.

</Syntax>
