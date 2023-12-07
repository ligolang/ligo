---
id: folding
title: Folding
---

import Syntax from '@theme/Syntax';

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is sometimes possible: *loops*
(see sections loops, sets and maps).

There are three kinds of functional iterations over lists: the *fold*,
the *map* (not to be confused with the *map data structure*) and the
*iteration*.

Let us consider first here the fold, which is the most general form of
functional iteration. The folded function takes two arguments: an
*accumulator* and the structure *element* at hand, with which it then
produces a new accumulator. This enables having a partial result that
becomes complete when the traversal of the data structure is over.

<Syntax syntax="cameligo">

The module `List` exports the functions `fold_left` and `fold_right`,
so folds have either the form:

```
List.fold_left folded init list
```

or

```
List.fold_right folded list init
```

which means that the folding can be done leftwards or rightwards on
the list. One way to tell them apart is to look where the folded
function, and the fold itself, keep the accumulator in their
signatures. Take for example a function `f`, a list `[1; 2; 3]`, and
an initial accumulator `init`. Then

```
List.fold_left f init [1; 2; 3] = f (f (f (init, 1), 2), 3)
```

and

```
List.fold_right f [1; 2; 3] init = f (1, f (2, f (3, init)))
```

  * The type of `List.fold_left` is `('a * 'b -> 'a) -> 'a -> 'b list
    -> 'a`.

  * The type of `List.fold_right` is `('b * 'a -> 'a) -> 'b list ->
    'a -> 'a`.

For example, let us compute the sum of integers in a list, assuming
that the empty list yields `0`:

```cameligo group=folding_lists
let sum1 = List.fold_left (fun (a,i) -> a + i) 0 [1; 2; 3]
let sum2 = List.fold_right (fun (i,a) -> i + a) [1; 2; 3] 0
```

> For OCaml users: In OCaml, the folded functions are curryfied, so
> their types are `('a -> 'b -> 'a)` `List.fold_left`, and `('b -> 'a
> -> 'a)` with `List.fold_right`.

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

The module `List` exports the functions `fold_left` and `fold_right`,
so folds have either the form:

```
List.fold_left (folded, init, list)
```

or

```
List.fold_right (folded, list, init)
```

which mean that the folding can be done leftwards or rightwards on the
list. One way to tell them apart is to look where the folded function,
and the fold itself, keep the accumulator in their signatures. Take
for example a function `f`, a list `list([1, 2, 3])`, and an initial
accumulator `init`. Then

```
List.fold_left (f, init, list([1;2;3])) = f (f (f (init, 1), 2), 3)
```

and

```
List.fold_right (f, list([1;2;3]), init) = f (1, (f (2, (f (3, init)))))
```

The type of `List.fold_left` is `(p : [a * b => a, a, b list]) => a`.

The type of `List.fold_right` is `(p : [b * a => a, b list, a]) => a`.

For example, let us compute the sum of integers in a list, assuming
that the empty list yields `0`:

```jsligo group=folding_lists
const add1 = ([a, i]) => a + i;
const sum1 = List.fold_left (add1, 0, list([1, 2, 3]));
const add2 = ([i, a]) => i + a;
const sum2 = List.fold_right (add2, list([1, 2, 3]), 0);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
