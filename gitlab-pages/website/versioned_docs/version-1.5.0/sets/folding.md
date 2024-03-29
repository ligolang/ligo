---
id: folding
title: Folding
---

import Syntax from '@theme/Syntax';

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is sometimes possible:
*loops*.

There are three kinds of functional iterations over sets: the *fold*,
the *map* (not to be confused with the *map data structure*) and the
*iteration*.

Let us consider first here the fold, which is the most general form of
functional iteration. The folded function takes two arguments: an
*accumulator* and the structure *element* at hand, with which it then
produces a new accumulator. This enables having a partial result that
becomes complete when the traversal of the data structure is over.

The function `Set.fold` performs a fold over a set, in increasing
order of its elements. The function `Set.fold_desc` folds in
decreasing order. The different in their types is the type of the
folded operation: with `Set.fold`, that function takes the accumulator
first, whereas with `Set.fold_desc`, the accumulator comes second.

<Syntax syntax="cameligo">

```cameligo group=set_folding
let s : int set = Set.literal [1; 2; 3]
// incr = [3; 2; 1]
let incr : int list = Set.fold (fun (a,i) -> i::a) s []
// decr = [1; 2; 3]
let decr : int list = Set.fold_desc (fun (i,a) -> i::a) s []
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_folding
const s: set<int> = Set.literal(list([1, 2, 3]));
// incr == list([3, 2, 1])
const incr: list<int> = Set.fold (([a,i]) => list([i,...a]), s, list([]));
// decr == list([1, 2, 3])
const decr: list<int> = Set.fold_desc (([i,a]) => list([i,...a]), s, list([]));
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
