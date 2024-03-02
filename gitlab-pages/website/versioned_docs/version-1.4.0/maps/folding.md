---
id: folding
title: Folding
---

import Syntax from '@theme/Syntax';

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is sometimes possible:
*loops*.

There are three kinds of functional iterations over maps: the *fold*,
the *map* (not to be confused with the *map data structure*) and the
*iteration*.

Let us consider first here the fold, which is the most general form of
functional iteration. The folded function takes two arguments: an
*accumulator* and the structure *element* at hand, with which it then
produces a new accumulator. This enables having a partial result that
becomes complete when the traversal of the data structure is over.

The function `Map.fold` performs a fold over the binding of a map, in
increasing order of its keys.

<Syntax syntax="cameligo">

```cameligo group=map_folding
type player = string
type abscissa = int
type ordinate = int
type move = abscissa * ordinate
type game = (player, move) map

let horizontal_offset (g : game) : int =
  let folded = fun (acc, j : int * (player * move)) -> acc + j.1.0
  in Map.fold folded g 0
```

Note: See the predefined
[module Map](../reference/map-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_folding
type player = string
type abscissa = int
type ordinate = int
type move = [abscissa, ordinate]
type game = map<player, move>

const horizontal_offset = (g: game): int => {
  let folded = ([acc, j]: [int, [player, move]]) => acc + j[1][0];
  return Map.fold(folded, g, 0);
};
```

Note: See the predefined
[namespace Map](../reference/map-reference/?lang=jsligo)

</Syntax>
