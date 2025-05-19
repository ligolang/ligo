---
title: Sets
---

import Syntax from '@theme/Syntax';

Sets are collections of elements of the same type without any duplicates.
Sets can be empty of contain any number of elements.

Sets are similar to  to [lists](./lists) because they are both collections of elements of the same type.
The main differences between sets and lists are:

- Sets cannot contain duplicate entries, while lists can
- Sets are always automatically sorted, while you can put the elements of a list in any order

Like lists, the type of sets is parameterised over the type of its elements.
Like list elements, set elements must all have the same type.

The empty set is denoted by the predefined value `Set.empty`.
To create a non-empty set, pass a list of literal values to the function `Set.literal`, which returns a set containing them, minus any duplicates.

<Syntax syntax="cameligo">

```cameligo group=sets
let empty_set : int set = Set.empty
let set1 : int set = Set.literal [3; 2; 2; 1]
```

:::note

The element `2` is repeated in the list, but not in the set made from it.
LIGO automatically removes duplicate elements in sets.

:::

To build a set from a list of arbitrary values (including variables and expressions, not just literal values), you must use `Set.of_list` instead of `Set.literal`, as in this example:

```cameligo group=sets
let two = 2
let set2 : int set = Set.of_list [3; two; two; two + two; 1]
```

For functions that work with sets, see the predefined [module Set](../reference/set-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
const empty_set: set<int> = Set.empty;
const set1: set<int> = Set.literal([3, 2, 2, 1]);
```

:::note

The element `2` is repeated in the list, but not in the set made from it.
LIGO automatically removed the duplicate.

:::

To build a set from a list of arbitrary values (including variables and expressions, not just literal values), you must use `Set.of_list` instead of `Set.literal`, as in this example:

```jsligo group=sets
const two = 2;
const set2 : set<int> = Set.of_list([3, two, two, two + two, 1]);
```

For functions that work with sets, see the predefined [namespace Set](../reference/set-reference/?lang=jsligo).

</Syntax>

Set elements are automatically sorted by increasing values, so the type of the elements must be *comparable*, that is, they obey a total order (any two elements can be compared).

## Sizing

The predefined functions `Set.size` and `Set.cardinal` return the number of elements in a given set.

<Syntax syntax="cameligo">

```cameligo group=cardinal
let my_set : int set = Set.literal [3; 2; 2; 1]
let cardinal : nat = Set.size my_set // = 3
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=cardinal
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const cardinal: nat = Set.size(my_set); // == 3
```

</Syntax>

## Searching

The function `Set.mem` tests for membership in a given set.

<Syntax syntax="cameligo">

```cameligo group=set_membership
let my_set : int set = Set.literal [3; 2; 2; 1]
let contains_3 : bool = Set.mem 3 my_set // = true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_membership
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const contains_3: bool = Set.mem(3, my_set); // == true
```

</Syntax>

## Adding elements

To add an element to a set, pass the element and the set to the `Set.add` function, which returns a new set.
If the element was already present in the given set, the resulting set is the same as the original one.

<Syntax syntax="cameligo">

```cameligo group=set_adding
let my_set : int set = Set.literal [3; 2; 2; 1]
let with_4 : int set = Set.add 4 my_set
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_adding
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const with_4: set<int> = Set.add(4, my_set);
```

</Syntax>

## Removing elements

To remove an element from a set, pass the element and the set to the `Set.remove` function, which returns a new set.
If the element is not already present, the new set is the same as the old one.

<Syntax syntax="cameligo">

```cameligo group=set_removing
let my_set : int set = Set.literal [3; 2; 2; 1]
let new_set = Set.remove 3 my_set
let contains_3 = Set.mem 3 new_set // = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_removing
const my_set: set<int> = Set.literal([3, 2, 2, 1]);
const new_set = Set.remove(3, my_set);
const contains_3 = Set.mem(3, new_set); // == false
```

</Syntax>

## Accessing elements

You cannot access elements directly in sets, such as getting the element with an arbitrary index.
The main way to get elements from sets is to use `Set.mem` to see if a specified element is in a set.
You can also use loops and the other functions listed below to process a set and access specific elements.

## Processing sets

Aside from adding and removing elements from sets, you can use loops and functions that iterate over every element in a set.

### Looping over sets

<Syntax syntax="cameligo">

There is no loop over sets in CameLIGO.

</Syntax>

<Syntax syntax="jsligo">

You can use a `for` loop to iterate over the elements in the set, in increasing order, in the form `for (const <variable> of <set>) <block>`.
This statement means that the block of statements (or a single statement) runs once for each element in the set (`<variable>`) ranging over the elements of the set from left to right.

Here is an example where the integers in a set are summed up.

```jsligo group=set_looping
function sum_elt (s: set<int>) {
  let sum = 0;
  for (const e of s) sum = sum + e;
  return sum;
};
```

</Syntax>

### Folding sets

Folding a set runs the same function on each element in a set and returns a single value.
The function takes two arguments: an *accumulator* and the current set element, with which it produces a new accumulator to pass to the next iteration of the function.
Folding lists allows you to compute a partial result that becomes complete when the traversal of the data structure is over.

Two ways to fold sets are available:

- The function `Set.fold` iterates over the set in increasing order of its elements.
- The function `Set.fold_desc` iterates over the set in decreasing order of its elements.

These functions take the same parameters, but the function that they run on the set elements is different.
In the `Set.fold` function, the first parameter is the accumulator and the second parameter is the current element in the set.
In the `Set.fold_desc` function, the first parameter is the current element in the set and the second parameter is the accumulator.

For a more detailed explanation of fold functions, see [Folding lists](./lists#folding-lists).

These examples use fold functions to turn a set into a list that is either sorted in increasing or decreasing order:

<Syntax syntax="cameligo">

```cameligo group=set_folding
let s : int set = Set.literal [1; 2; 3]
// incr = [3; 2; 1]
let incr : int list = Set.fold (fun (a,i) -> i::a) s []
// decr = [1; 2; 3]
let decr : int list = Set.fold_desc (fun (i,a) -> i::a) s []
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_folding
const s: set<int> = Set.literal([1, 2, 3]);
// incr == [3, 2, 1]
const incr: list<int> = Set.fold (([a,i]) => ([i,...a] as list<int>), s, []);
// decr == [1, 2, 3]
const decr: list<int> = Set.fold_desc (([i,a]) => ([i,...a] as list<int>), s, []);
```

</Syntax>

### Mapping sets

Mapping a set runs the same function on each element in a set and returns a new set with the result of each function operation.
This is called a *map operation*, not to be confused with the map data type.
To map a set, use the `Set.map` function, as in this example:

<Syntax syntax="cameligo">

```cameligo group=set_mapping
let s : int set = Set.literal [5; 1; 2; 2]
// plus_one = Set.literal [6; 2; 3]
let plus_one : int set = Set.map (fun i -> i + 1) s
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_mapping
const s: set<int> = Set.literal([5,1,2,2]);
// plus_one == Set.literal([6,2,3])
const plus_one: set<int> = Set.map(i => i + 1, s);
```

</Syntax>

### Updating elements

Previous sections show how to add and remove an element from a given set.
The function `Set.update` can do both depending on a boolean value: if true, then the given value is added to the set, otherwise it is removed (if present).

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

Similarly, the function `Set.filter_map` applies the same function to every element in a set.
If the function returns a `None` option, the element stays the same, but if it returns a `Some` option, the element is replaced with the value in the option.

This example uses `Set.filter_map` to remove all even numbers from a set:

<Syntax syntax="cameligo">

```cameligo group=set_updating
let f x = if x mod 2 = 0n then None else Some x
// odds = Set.literal [3, 1]
let odds = Set.filter_map f nats
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_updating
const f = x => x % 2 == (0 as nat) ?
               ["None" as "None"] :
               ["Some" as "Some", x];
// odds == Set.literal([3, 1])
const odds = Set.filter_map(f, nats);
```

</Syntax>

### Iterating over sets

The `Set.iter` function is similar to the `Set.map` function because it runs the same function on every element in a set.
However, the `Set.iter` function returns a value of type unit, so it cannot change the set.
Therefore, this function is useful only to produce side effects, such as checking that each element of a set is within a certain range, and fail with an error otherwise.

This example iterates over a set to check that all its elements (integers) are greater than 3:

<Syntax syntax="cameligo">

```cameligo group=set_iterating
let assert_all_greater_than_3 (s : int set) : unit =
  Set.iter (fun i -> Assert.assert (i > 3)) s
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=set_iterating
const assert_all_greater_than_3 =
  (s: set<int>): unit => Set.iter(i => Assert.assert(i > 3), s);
```

</Syntax>
