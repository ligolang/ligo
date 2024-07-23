---
title: Sets
---

import Syntax from '@theme/Syntax';

Sets are unordered collections of values of the same type, like lists
are ordered collections. Like the mathematical sets and lists, sets
can be empty and, if not, elements of sets in LIGO are *unique*,
whereas they can be repeated in a *list*.

Like lists, the type of sets is parameterised over the type of its
elements. Like list elements, set elements must all have the same
type.

The empty set is denoted by the predefined value `Set.empty`. A
non-empty set can be built by using the function `Set.literal` which
takes a list of *literal elements* and returns a set containing them,
and only them.

<Syntax syntax="cameligo">

```cameligo group=sets
let empty_set : int set = Set.empty
let set1 : int set = Set.literal [3; 2; 2; 1]
```

> Note: The element `2` is repeated in the list, but not in the set
> made from it.

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
const empty_set: set<int> = Set.empty;
const set1: set<int> = Set.literal([3, 2, 2, 1]);
```

> Note: The element `2` is repeated in the list, but not in the set
> made from it.

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

If you want to build a big set from an arbitrary list of arbitrary
values (not just literal values), then you must use `Set.of_list`
instead of `Set.literal`:

```jsligo group=sets
const two = 2;
const set2 : set<int> = Set.of_list([3, two, two, 1]);
```
</Syntax>

Set elements are internally sorted by increasing values, so the type
of the elements must be *comparable*, that is, they obey a total order
(any two elements can be compared).

## Sizing

The predefined functions `Set.size` and `Set.cardinal` return the
number of elements in a given set.

<Syntax syntax="cameligo">

```cameligo group=cardinal
let my_set : int set = Set.literal [3; 2; 2; 1]
let cardinal : nat = Set.size my_set // = 3
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=cardinal
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const cardinal : nat = Set.size(my_set); // == 3
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Searching

The predicate `Set.mem` tests for membership in a given set.

<Syntax syntax="cameligo">

```cameligo group=set_membership
let my_set : int set = Set.literal [3; 2; 2; 1]
let contains_3 : bool = Set.mem 3 my_set // = true
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_membership
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const contains_3: bool = Set.mem(3, my_set); // == true
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Adding

Adding an element to a set is done by calling the function
`Set.add`. If the element was already present in the given set, the
resulting set is the same as the original one.

<Syntax syntax="cameligo">

```cameligo group=set_adding
let my_set : int set = Set.literal [3; 2; 2; 1]
let with_4 : int set = Set.add 4 my_set
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_adding
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const with_4: set<int> = Set.add(4, my_set);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Removing

The function `Set.remove` creates a set containing the elements of a
given set, without a given element. If the element is not already
present, the new set is the same as the old one, as expected.

<Syntax syntax="cameligo">

```cameligo group=set_removing
let my_set : int set = Set.literal [3; 2; 2; 1]
let new_set = Set.remove 3 my_set
let contains_3 = Set.mem 3 new_set // = false
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_removing
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const new_set = Set.remove(3, my_set);
const contains_3 = Set.mem(3, new_set); // == false
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Updating

Previous sections show how to add and remove an element from a given
set. The function `Set.update` can do both depending on a boolean
value: if true, then the given value will be added to the set,
otherwise it will be removed (if present).

<Syntax syntax="cameligo">

```cameligo group=set_updating
let nats : int set = Set.literal [3; 2; 2; 1]
let set_with_5 = Set.update 5 true nats
let set_without_3 = Set.update 3 false nats
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_updating
const nats: set<int> = Set.literal([3, 2, 2, 1]);
const set_with_5 = Set.update(5, true, nats);
const set_without_3 = Set.update(3, false, nats);
```

</Syntax>

The function `Set.update` implements a one-value update. Sometime we
would like to provide a function that is applied in turn to *all* the
elements of the set, and specifies whether the element at hand has to
be discarded or replaced by a computed value. This is what
`Set.filter_map` does.

As an example, let us consider a function that removes all the even
numbers from a set.

<Syntax syntax="cameligo">

```cameligo group=set_updating
let f x = if x mod 2 = 0n then None else Some x
// odds = Set.literal [3, 1]
let odds = Set.filter_map f nats
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_updating
const f = x => x % 2 == 0n ? None() : Some(x);
// odds == Set.literal([3, 1])
const odds = Set.filter_map(f, nats);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Folding

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
const s : set<int> = Set.literal([1, 2, 3]);
// incr == [3, 2, 1]
const incr : list<int> = Set.fold (([a,i]) => ([i,...a] as list<int>), s, []);
// decr == [1, 2, 3]
const decr : list<int> = Set.fold_desc (([i,a]) => ([i,...a] as list<int>), s, []);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Mapping

We may want to change all the elements of a given set by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over sets is called `Set.map` and is
used as follows.

<Syntax syntax="cameligo">

```cameligo group=set_mapping
let s : int set = Set.literal [5; 1; 2; 2]
// plus_one = Set.literal [6; 2; 3]
let plus_one : int set = Set.map (fun i -> i + 1) s
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_mapping
const s: set<int> = Set.literal([5,1,2,2]);
// plus_one == Set.literal([6,2,3])
const plus_one: set<int> = Set.map(i => i + 1, s);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>


## Iterating

An *iterated operation* is a fold over a set that returns the value of
type `unit`, that is, its only use is to produce side-effects. This
can be useful if, for example, you would like to check that each
element of a set is within a certain range, and fail with an error
otherwise.

The predefined functional iterator implementing the iterated operation
over sets is called `Set.iter`. In the following example, a set is
iterated to check that all its elements (integers) are greater than
`3`.

<Syntax syntax="cameligo">

```cameligo group=set_iterating
let assert_all_greater_than_3 (s : int set) : unit =
  Set.iter (fun i -> assert (i > 3)) s
```

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_iterating
const assert_all_greater_than_3 =
  (s: set<int>) : unit => Set.iter(i => assert(i > 3), s);
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>

## Looping

<Syntax syntax="cameligo">
There is no loop over lists in CameLIGO.

Note: See the predefined
[module Set](../reference/set-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

One can iterate through all the elements of a set, in increasing
order, thanks to a loop of the form `for (const <variable> of <set>) <block>`. It means that the `<block>` of statements (or a single
statement) will be computed once for each `<variable>` ranging over the
elements of the set `<set>` in increasing order.

Here is an example where the integers in a set are summed up.

```jsligo group=set_looping
function sum_elt (s: set<int>) {
  let sum = 0;
  for (const e of s) sum = sum + e;
  return sum;
};
```

Note: See the predefined
[namespace Set](../reference/set-reference/?lang=jsligo)

</Syntax>
