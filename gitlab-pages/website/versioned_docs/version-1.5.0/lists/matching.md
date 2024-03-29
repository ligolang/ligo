---
id: matching
title: Matching
---

import Syntax from '@theme/Syntax';

Polymorphism is especially useful when writing functions over
parametric types, which include built-in types like lists, sets, and
maps.

As an example, we will see how to implement list reversing
parametrically on any type, rather than just on lists of a specific
type.

Similarly to the polymorphic identity function, we can introduce a
type variable that can be generalised. We will write a direct version
of the function using an accumulator.

<Syntax syntax="cameligo">

```cameligo group=reverse
let rev (type a) (xs : a list) : a list =
  let rec rev (type a) (xs : a list) (acc : a list) : a list =
    match xs with
    | [] -> acc
    | x :: xs -> rev xs (x::acc)
  in rev xs []
```

Note that because the type variable `a` was introduced (bound) by
means of `type`, it does not need a quote, like `'a`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=reverse
function rev <T>(xs : list<T>) : list<T> {
  const rev = <T>([xs, acc] : [list<T>, list<T>]) : list<T> =>
    match(xs) {
      when([]): acc;
      when([y,...ys]): rev([ys, list([y,...acc])])
    };

  return rev([xs, list([])]);
};
```

Note how the type checker was able to infer the types of `[]` and
`[y,...ys]` in the `when` clauses (without the need of using
`list([])` and `list([y,...ys])`), but in `list([y,...acc])` the cast
to `list` is necessary, because of the rest property that needs to be
interpreted as a cons. Similarly, the `list` in `[xs, list([])]` is
needed to force the interpretation of `[]` as the empty list, instead
of the empty array ("unit").

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

We use an accumulator variable `acc` to keep the elements of the list
processed, consing each element on it.

As with the identity function, we can then use `rev` directly with
different type instances:

<Syntax syntax="cameligo">

```cameligo group=reverse
let ints : int list = rev [1; 2; 3]
let nats : nat list = rev [1n; 2n; 3n]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=reverse
const ints : list<int> = rev(list([1, 2, 3]));
const nats : list<nat> = rev(list([1n, 2n, 3n]));
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
