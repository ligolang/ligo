---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

Sets are unordered collections of values of the same type, like lists
are ordered collections. Like the mathematical sets and lists, sets
can be empty and, if not, elements of sets in LIGO are *unique*,
whereas they can be repeated in a *list*.

Like lists, the type of sets is parameterised over the type of its
elements. Like list elements, set elements must all have the same
type.

Ordinary sets are fine for contracts with a finite lifespan or a
bounded number of users. For many contracts however, the intention is
to have a set holding *many* entries, potentially millions of
them. The cost of loading those entries into the environment each time
a user executes the contract would eventually become too expensive
were it not for *big sets*. Big sets in LIGO are based on *big maps*,
a data structure offered by Michelson which handles the scaling
concerns for us. The interface for big sets closer to that of big maps
than ordinary sets (for example, it would defeat the purpose to
request the size of a big set).

<Syntax syntax="cameligo">

The type of big sets is `'elt big_set` or, equivalently, `'elt
Big_set.`, where `'elt` is the type of the elements of the big set. It
is defined as follows in the standard library:

```cameligo group=big_sets
type 'elt t = ('elt, unit) big_map
```

The empty big set is denoted by the predefined value
`Big_set.empty`. In some contexts, it is useful to annotate it with
its type, for example: `(empty : int Big_set.t)`.

A non-empty big set can be built by using the function
`Big_set.literal` which takes a list of *literal elements* and returns
a set containing them, and only them.

```cameligo group=big_sets
let empty_big_set : int big_set = Big_set.empty
let big_set1 : int big_set = Big_set.literal [3; 2; 2; 1]
```

> Note: The element `2` is repeated in the list, but not in the set
> made from it.

If you want to build a big set from an arbitrary list of arbitrary
values (not just literal values), then you must use `Big_set.of_list`
instead of `Big_set.literal`:

```cameligo group=big_sets
let two = 2
let big_set2 : int big_set = Big_set.of_list [3; two; two; 1]
```

</Syntax>

<Syntax syntax="jsligo">

The type of big sets is `big_set<elt>` or, equivalently,
`Big_set.t<elt>`, where `elt` is the type of the elements of the big
set. It is defined as follows in the standard library:

```jsligo group=big_sets
type t<elt> = big_map<elt, unit>
```

The empty big set is denoted by the predefined value
`Big_set.empty`. In some contexts, it is useful to annotate it with
its type, for example: `(empty as Big_set.t<int>)`.

A non-empty big set can be built by using the function
`Big_set.literal` which takes a list of elements and returns a set
containing them, and only them.


```jsligo group=big_sets
const empty_big_set: big_set<int> = Big_set.empty;
const big_set1: big_set<int> = Big_set.literal(list([3, 2, 2, 1]));
```

> Note: The element `2` is repeated in the list, but not in the set
> made from it.

If you want to build a big set from an arbitrary list of arbitrary
values (not just literal values), then you must use `Big_set.of_list`
instead of `Big_set.literal`:

```jsligo group=big_sets
const two = 2;
const big_set2: big_set<int> = Big_set.of_list(list([3, two, two, 1]));
```

</Syntax>

Set elements are internally sorted by increasing values, so the type
of the elements must be *comparable*, that is, they obey a total order
(any two elements can be compared).
