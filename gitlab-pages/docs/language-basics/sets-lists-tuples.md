---
id: sets-lists-tuples
title: Tuples, Lists, Sets
---

import Syntax from '@theme/Syntax';

Apart from complex data types such as `maps` and `records`, LIGO also
features `tuples`, `lists` and `sets`.

## Tuples

Tuples gather a given number of values in a specific order and those
values, called *components*, can be retrieved by their index
(position).  Probably the most common tuple is the *pair*. For
example, if we were storing coordinates on a two dimensional grid we
might use a pair `(x,y)` to store the coordinates `x` and `y`. There
is a *specific order*, so `(y,x)` is not equal to `(x,y)` in
general. The number of components is part of the type of a tuple, so,
for example, we cannot add an extra component to a pair and obtain a
triple of the same type: `(x,y)` has always a different type from
`(x,y,z)`, whereas `(y,x)` might have the same type as `(x,y)`.

Like records, tuple components can be of arbitrary types.

### Defining Tuples

Unlike [a record](language-basics/maps-records.md), tuple types do not
have to be defined before they can be used. However below we will give
them names by *type aliasing*.



<Syntax syntax="pascaligo">

```pascaligo group=tuple
type full_name is string * string  // Alias

const full_name : full_name = ("Alice", "Johnson")
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=tuple
type full_name = string * string  // Alias

let full_name : full_name = ("Alice", "Johnson") // Optional parentheses
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=tuple
type full_name = (string, string);  // Alias

let full_name : full_name = ("Alice", "Johnson");
```

</Syntax>



### Accessing Components

Accessing the components of a tuple in OCaml is achieved by
[pattern matching](language-basics/unit-option-pattern-matching.md). LIGO
currently supports tuple patterns only in the parameters of functions,
not in pattern matching. However, we can access components by their
position in their tuple, which cannot be done in OCaml. *Tuple
components are zero-indexed*, that is, the first component has index
`0`.



<Syntax syntax="pascaligo">

```pascaligo group=tuple
const first_name : string = full_name.0
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=tuple
let first_name : string = full_name.0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=tuple
let first_name : string = full_name[0];
```

</Syntax>


## Lists

Lists are linear collections of elements of the same type. Linear
means that, in order to reach an element in a list, we must visit all
the elements before (sequential access). Elements can be repeated, as
only their order in the collection matters. The first element is
called the *head*, and the sub-list after the head is called the
*tail*. For those familiar with algorithmic data structure, you can
think of a list a *stack*, where the top is written on the left.

> 💡 Lists are needed when returning operations from a smart
> contract's main function.

### Defining Lists


<Syntax syntax="pascaligo">

```pascaligo group=lists
const empty_list : list (int) = nil // Or list []
const my_list : list (int) = list [1; 2; 2] // The head is 1
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let empty_list : int list = []
let my_list : int list = [1; 2; 2] // The head is 1
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let empty_list : list (int) = [];
let my_list : list (int) = [1, 2, 2]; // The head is 1
```

</Syntax>


### Adding to Lists

Lists can be augmented by adding an element before the head (or, in
terms of stack, by *pushing an element on top*). This operation is
usually called *consing* in functional languages.



<Syntax syntax="pascaligo">

In PascaLIGO, the *cons operator* is infix and noted `#`. It is not
symmetric: on the left lies the element to cons, and, on the right, a
list on which to cons. (The symbol is helpfully asymmetric to remind
you of that.)

```pascaligo group=lists
const larger_list : list (int) = 5 # my_list // [5;1;2;2]
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the *cons operator* is infix and noted `::`. It is not
symmetric: on the left lies the element to cons, and, on the right, a
list on which to cons.

```cameligo group=lists
let larger_list : int list = 5 :: my_list // [5;1;2;2]
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the *cons operator* is infix and noted `, ...`. It is
not symmetric: on the left lies the element to cons, and, on the
right, a list on which to cons.

```reasonligo group=lists
let larger_list : list (int) = [5, ...my_list]; // [5,1,2,2]
```

</Syntax>


### Functional Iteration over Lists

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO lists: the
*iterated operation*, the *map operation* (not to be confused with the
*map data structure*) and the *fold operation*.

#### Iterated Operation over Lists

The first, the *iterated operation*, is an iteration over the list
with a unit return value. It is useful to enforce certain invariants
on the element of a list, or fail.

For example you might want to check that each value inside of a list
is within a certain range, and fail otherwise. The predefined
functional iterator implementing the iterated operation over lists is
called `List.iter`.

In the following example, a list is iterated to check that all its
elements (integers) are strictly greater than `3`.



<Syntax syntax="pascaligo">

```pascaligo group=lists
function iter_op (const l : list (int)) : unit is
  block {
    function iterated (const i : int) : unit is
      if i > 3 then Unit else (failwith ("Below range.") : unit)
  } with List.iter (iterated, l)
```

> Note that `list_iter` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let iter_op (l : int list) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in List.iter predicate l
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let iter_op = (l : list (int)) : unit => {
  let predicate = (i : int) => assert (i > 3);
  List.iter (predicate, l);
};
```

</Syntax>



#### Mapped Operation over Lists

We may want to change all the elements of a given list by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over lists is called `List.map` and
is used as follows.



<Syntax syntax="pascaligo">

```pascaligo group=lists
function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = List.map (increment, larger_list)
```

> Note that `list_map` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let increment = (i : int) : int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list (int) = List.map (increment, larger_list);
```

</Syntax>



#### Folded Operation over Lists

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over. The predefined functional
iterator implementing the folded operation over lists is called
`List.fold` and is used as follows.



<Syntax syntax="pascaligo">

```pascaligo group=lists
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = List.fold (sum, my_list, 0)
```

> Note that `list_fold` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=lists
let sum (acc, i: int * int) : int = acc + i
let sum_of_elements : int = List.fold sum my_list 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=lists
let sum = ((result, i): (int, int)): int => result + i;
let sum_of_elements : int = List.fold (sum, my_list, 0);
```

</Syntax>


## Sets

Sets are unordered collections of values of the same type, like lists
are ordered collections. Like the mathematical sets and lists, sets
can be empty and, if not, elements of sets in LIGO are *unique*,
whereas they can be repeated in a *list*.

### Empty Sets



<Syntax syntax="pascaligo">

In PascaLIGO, the notation for sets is similar to that for lists,
except the keyword `set` is used before:

```pascaligo group=sets
const my_set : set (int) = set []
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the empty set is denoted by the predefined value
`Set.empty`.

```cameligo group=sets
let my_set : int set = Set.empty
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the empty set is denoted by the predefined value
`Set.empty`.

```reasonligo group=sets
let my_set : set (int) = Set.empty;
```

</Syntax>


### Non-empty Sets



<Syntax syntax="pascaligo">

In PascaLIGO, the notation for sets is similar to that for lists,
except the keyword `set` is used before:

```pascaligo group=sets
const my_set : set (int) = set [3; 2; 2; 1]
```
You can check that `2` is not repeated in `my_set` by using the LIGO
compiler like this (the output will sort the elements of the set, but
that order is not significant for the compiler):
```shell
ligo evaluate-value
gitlab-pages/docs/language-basics/src/sets-lists-tuples/sets.ligo my_set
# Outputs: { 3 ; 2 ; 1 }
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, there is no predefined syntactic construct for sets: you
must build your set by adding to the empty set. (This is the way in
OCaml.)

```cameligo group=sets
let my_set : int set =
  Set.add 3 (Set.add 2 (Set.add 2 (Set.add 1 (Set.empty : int set))))
```
You can check that `2` is not repeated in `my_set` by using the LIGO
compiler like this (the output will sort the elements of the set, but
that order is not significant for the compiler):

```shell
ligo evaluate-value
gitlab-pages/docs/language-basics/src/sets-lists-tuples/sets.mligo my_set
# Outputs: { 3 ; 2 ; 1 }
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, there is no predefined syntactic construct for sets:
you must build your set by adding to the empty set. (This is the way
in OCaml.)

```reasonligo group=sets
let my_set : set (int) =
  Set.add (3, Set.add (2, Set.add (2, Set.add (1, Set.empty : set (int)))));
```

You can check that `2` is not repeated in `my_set` by using the LIGO
compiler like this (the output will sort the elements of the set, but
that order is not significant for the compiler):

```shell
ligo evaluate-value
gitlab-pages/docs/language-basics/src/sets-lists-tuples/sets.religo my_set
# Outputs: { 3 ; 2 ; 1 }
```

</Syntax>


### Set Membership



<Syntax syntax="pascaligo">

PascaLIGO features a special keyword `contains` that operates like an
infix operator checking membership in a set.

```pascaligo group=sets
const contains_3 : bool = my_set contains 3
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, the predefined predicate `Set.mem` tests for membership
in a set as follows:

```cameligo group=sets
let contains_3 : bool = Set.mem 3 my_set
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, the predefined predicate `Set.mem` tests for membership
in a set as follows:

```reasonligo group=sets
let contains_3 : bool = Set.mem (3, my_set);
```

</Syntax>



### Cardinal of Sets

The predefined function `Set.size` returns the number of
elements in a given set as follows.



<Syntax syntax="pascaligo">

```pascaligo group=sets
const cardinal : nat = Set.size (my_set)
```

> Note that `size` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let cardinal : nat = Set.size my_set
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sets
let cardinal : nat = Set.size (my_set);
```

</Syntax>



### Updating Sets

There are two ways to update a set, that is to add or remove from
it.



<Syntax syntax="pascaligo">

In PascaLIGO, either we create a new set from the given one, or we
modify it in-place. First, let us consider the former way:

```pascaligo group=sets
const larger_set  : set (int) = Set.add (4, my_set)
const smaller_set : set (int) = Set.remove (3, my_set)
```

> Note that `set_add` and `set_remove` are *deprecated*.

If we are in a block, we can use an instruction to modify the set
bound to a given variable. This is called a *patch*. It is only
possible to add elements by means of a patch, not remove any: it is
the union of two sets.

In the following example, the parameter set `s` of function `update`
is augmented (as the `with s` shows) to include `4` and `7`, that is,
this instruction is equivalent to perform the union of two sets, one
that is modified in-place, and the other given as a literal
(extensional definition).

```pascaligo group=sets
function update (var s : set (int)) : set (int) is block {
  patch s with set [4; 7]
} with s

const new_set : set (int) = update (my_set)
```

</Syntax>
<Syntax syntax="cameligo">

In CameLIGO, we can use the predefined functions `Set.add` and
`Set.remove`. We update a given set by creating another one, with or
without some elements.

```cameligo group=sets
let larger_set  : int set = Set.add 4 my_set
let smaller_set : int set = Set.remove 3 my_set
```

</Syntax>
<Syntax syntax="reasonligo">

In ReasonLIGO, we can use the predefined functions `Set.add` and
`Set.remove`. We update a given set by creating another one, with or
without some elements.

```reasonligo group=sets
let larger_set  : set (int) = Set.add (4, my_set);
let smaller_set : set (int) = Set.remove (3, my_set);
```

</Syntax>



### Functional Iteration over Sets

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO maps: the
*iterated operation*, the *mapped operation* (not to be confused with
the *map data structure*) and the *folded operation*.

#### Iterated Operation

The first, the *iterated operation*, is an iteration over the map with
no return value: its only use is to produce side-effects. This can be
useful if for example you would like to check that each value inside
of a map is within a certain range, and fail with an error otherwise.

The predefined functional iterator implementing the iterated operation
over sets is called `Set.iter`. In the following example, a set is
iterated to check that all its elements (integers) are greater than
`3`.



<Syntax syntax="pascaligo">

```pascaligo group=sets
function iter_op (const s : set (int)) : unit is
  block {
    function iterated (const i : int) : unit is
      if i > 2 then Unit else (failwith ("Below range.") : unit)
  } with Set.iter (iterated, s)
```

> Note that `set_iter` is *deprecated*.

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let iter_op (s : int set) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in Set.iter predicate s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sets
let iter_op = (s : set (int)) : unit => {
  let predicate = (i : int) => assert (i > 3);
  Set.iter (predicate, s);
};
```

</Syntax>



<!-- #### Mapped Operation (NOT IMPLEMENTED YET) -->

<!-- We may want to change all the elements of a given set by applying to -->
<!-- them a function. This is called a *mapped operation*, not to be -->
<!-- confused with the map data structure. -->

<!-- <\!--DOCUSAURUS_CODE_TABS-\-> -->

<!-- <\!--PascaLIGO-\-> -->

<!-- In PascaLIGO, the predefined functional iterator implementing the -->
<!-- mapped operation over sets is called `Set.map` and is used as follows: -->

<!-- ```pascaligo skip -->
<!-- function increment (const i : int): int is i + 1 -->

<!-- // Creates a new set with all elements incremented by 1 -->
<!-- const plus_one : set (int) = Set.map (increment, larger_set) -->
<!-- ``` -->

<!-- <\!--CameLIGO-\-> -->

<!-- In CameLIGO, the predefined functional iterator implementing the -->
<!-- mapped operation over sets is called `Set.map` and is used as follows: -->

<!-- ```cameligo skip -->
<!-- let increment (i : int) : int = i + 1 -->

<!-- // Creates a new set with all elements incremented by 1 -->
<!-- let plus_one : int set = Set.map increment larger_set -->
<!-- ``` -->

<!-- <\!--ReasonLIGO-\-> -->

<!-- In ReasonLIGO, the predefined functional iterator implementing the -->
<!-- mapped operation over sets is called `Set.map` and is used as follows: -->

<!-- ```reasonligo skip -->
<!-- let increment = (i : int) : int => i + 1; -->

<!-- // Creates a new set with all elements incremented by 1 -->
<!-- let plus_one : set (int) = Set.map (increment, larger_set); -->
<!-- ``` -->

<!-- <\!--END_DOCUSAURUS_CODE_TABS-\-> -->

#### Folded Operation

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over. The predefined fold over sets
is called `Set.fold`.


<Syntax syntax="pascaligo">

```pascaligo group=sets
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = Set.fold (sum, my_set, 0)
```

> Note that `set_fold` is *deprecated*.

It is possible to use a *loop* over a set as well.

```pascaligo group=sets
function loop (const s : set (int)) : int is block {
  var sum : int := 0;
  for element in set s block {
    sum := sum + element
  }
} with sum
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let sum (acc, i : int * int) : int = acc + i
let sum_of_elements : int = Set.fold sum my_set 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sets
let sum = ((acc, i) : (int, int)) : int => acc + i;
let sum_of_elements : int = Set.fold (sum, my_set, 0);
```

</Syntax>

