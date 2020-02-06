---
id: sets-lists-tuples
title: Tuples, Lists, Sets
---

Apart from complex data types such as `maps` and `records`, ligo also
exposes `tuples`, `lists` and `sets`.

> ⚠️ Make sure to pick the appropriate data type for your use case, and
> bear in mind the related gas costs.

## Tuples

Tuples gather a given number of values in a specific order and those
values, called *components*, can be retrieved by their index
(position).  Probably the most common tuple is the *pair*. For
example, if we were storing coordinates on a two dimensional grid we
might use a pair of type `int * int` to store the coordinates `x` and
`y` as the pair value `(x,y)`. There is a *specific order*, so `(y,x)`
is not equal to `(x,y)`. The number of components is part of the type
of a tuple, so, for example, we cannot add an extra component to a
pair and obtain a triple of the same type: `(x,y)` has always a
different type from `(x,y,z)`, whereas `(y,x)` may have the same type.

Like records, tuple components can be of arbitrary types.

### Defining a tuple

Unlike [a record](language-basics/maps-records.md), tuple types do not
have to be defined before they can be used. However below we will give
them names by *type aliasing*.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->
```pascaligo group=c
type full_name is string * string  // Alias

const full_name : full_name = ("Alice", "Johnson")
```

<!--CameLIGO-->
```cameligo group=c
type full_name = string * string  // Alias

(* The parenthesis here are optional *)
let full_name : full_name = ("Alice", "Johnson")
```

<!--ReasonLIGO-->
```reasonligo group=c
type full_name = (string, string);  // Alias

(* The parenthesis here are optional *)
let full_name : full_name = ("Alice", "Johnson");
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Accessing an Element in a Tuple

Accessing the components of a tuple in OCaml is achieved by
[pattern matching](language-basics/unit-option-pattern-matching.md). LIGO
currently supports tuple patterns only in the parameters of functions,
not in pattern matching. In LIGO, however, we can access components by
their position in their tuple, which cannot be done in OCaml.

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

Tuple components are one-indexed like so:

```pascaligo group=c
const first_name : string = full_name.1;
```

<!--Cameligo-->

Tuple elements are zero-indexed and accessed like so:

```cameligo group=c
let first_name : string = full_name.0
```

<!--ReasonLIGO-->

Tuple components are one-indexed like so:

```reasonligo group=c
let first_name : string = full_name[1];
```

## Lists

Lists are linear collections of elements of the same type. Linear
means that, in order to reach an element in a list, we must visit all
the elements before (sequential access). Elements can be repeated, as
only their order in the collection matters. The first element is
called the *head*, and the sub-list after the head is called the
*tail*. For those familiar with algorithmic data structure, you can
think of a list a *stack*, where the top is written on the left.

> 💡 Lists are useful when returning operations from a smart
> contract's entrypoint.

### Defining a List

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
const my_list : list (int) = list [1; 2; 2] // The head is 1
```

<!--CameLIGO-->
```cameligo group=b
let my_list : int list = [1; 2; 2] // The head is 1
```

<!--ReasonLIGO-->
```reasonligo group=b
let my_list : list (int) = [1, 2, 2]; // The head is 1
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Adding to a List


Lists can be augmented by adding an element before the head (or, in
terms of stack, by *pushing an element on top*). This operation is
usually called *consing* in functional languages.

In PascaLIGO, the *cons operator* is infix and noted `#`. It is not
symmetric: on the left lies the element to cons, and, on the right, a
list on which to cons. (The symbol is helpfully asymmetric to remind
you of that.)

In CameLIGO, the *cons operator* is infix and noted `::`. It is not
symmetric: on the left lies the element to cons, and, on the right, a
list on which to cons.

In ReasonLIGO, the *cons operator* is infix and noted `, ...`. It is
not symmetric: on the left lies the element to cons, and, on the
right, a list on which to cons.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
const larger_list : list (int) = 5 # my_list
```

<!--CameLIGO-->
```cameligo group=b
let larger_list : int list = 5 :: my_list
```

<!--ReasonLIGO-->
```reasonligo group=b
let larger_list : list (int) = [5, ...my_list];
```
<!--END_DOCUSAURUS_CODE_TABS-->

<br/>

> 💡 Lists can be iterated, folded or mapped to different values. You
> can find additional examples
> [here](https://gitlab.com/ligolang/ligo/tree/dev/src/test/contracts)
> and other built-in operators
> [here](https://gitlab.com/ligolang/ligo/blob/dev/src/passes/operators/operators.ml#L59)

### Mapping of a List

We may want to apply a function to all the elements of a list and
obtain the resulting list, in the same order. For example, we may want
to create a list that contains all the elements of another list
incremented by one. This is a special case of *fold operation* called
a *map operation*. Map operations (not to be confused by the
[map data structure](language-basics/maps-records.md)), are predefined
functions in LIGO. They take as a parameter the function to apply to
all the elements. Of course, that function must return a value of the
same type as the element.

In PascaLIGO, the map function is called `list_map`.

In CameLIGO and ReasonLIGO, the map function is called `List.map`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = list_map (increment, larger_list)
```
<!--CameLIGO-->
```cameligo group=b
let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```
<!--ReasonLIGO-->
```reasonligo group=b
let increment = (i : int) : int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list (int) = List.map (increment, larger_list);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Folding of over a List

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
function sum (const acc : int; const i : int): int is acc + i

const sum_of_elements : int = list_fold (sum, my_list, 0)
```

<!--CameLIGO-->

```cameligo group=b
let sum (acc, i: int * int) : int = acc + i
let sum_of_elements : int = List.fold sum my_list 0
```

<!--ReasonLIGO-->

```reasonligo group=b
let sum = ((result, i): (int, int)): int => result + i;
let sum_of_elements : int = List.fold (sum, my_list, 0);
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Sets

Sets are unordered collections of values of the same type, like lists
are ordered collections. Like the mathematical sets and lists, sets
can be empty and, if not, elements of sets in LIGO are *unique*,
whereas they can be repeated in a list.

### Empty Sets

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const my_set : set (int) = set []
```
<!--CameLIGO-->
```cameligo group=a
let my_set : int set = (Set.empty : int set)
```
<!--ReasonLIGO-->
```reasonligo group=a
let my_set : set (int) = (Set.empty : set (int));
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Non-empty Sets



In PascaLIGO, the notation for sets is similar to that for lists,
except the keyword `set` is used before:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const my_set : set (int) = set [3; 2; 2; 1]
```
<!--END_DOCUSAURUS_CODE_TABS-->

You can check that `2` is not repeated in `my_set` by using the LIGO
compiler like this (the output will sort the elements of the set, but
that order is not significant for the compiler):

```shell
ligo evaluate-value
gitlab-pages/docs/language-basics/src/sets-lists-tuples/sets.ligo my_set
# Outputs: { 3 ; 2 ; 1 }
```

In CameLIGO, there is no predefined syntactic construct for sets: you
must build your set by adding to the empty set. (This is the way in
OCaml.)

<!--DOCUSAURUS_CODE_TABS-->
<!--CameLIGO-->
```cameligo group=a
let my_set : int set =
  Set.add 3 (Set.add 2 (Set.add 2 (Set.add 1 (Set.empty : int set))))
```
<!--END_DOCUSAURUS_CODE_TABS-->

You can check that `2` is not repeated in `my_set` by using the LIGO
compiler like this (the output will sort the elements of the set, but
that order is not significant for the compiler):

```shell
ligo evaluate-value
gitlab-pages/docs/language-basics/src/sets-lists-tuples/sets.mligo my_set
# Outputs: { 3 ; 2 ; 1 }
```

In ReasonLIGO, there is no predefined syntactic construct for sets:
you must build your set by adding to the empty set. (This is the way
in OCaml.)

<!--DOCUSAURUS_CODE_TABS-->
<!--ReasonLIGO-->
```reasonligo group=a
let my_set : set (int) =
  Set.add (3, Set.add (2, Set.add (2, Set.add (1, Set.empty : set (int)))));
```
<!--END_DOCUSAURUS_CODE_TABS-->

You can check that `2` is not repeated in `my_set` by using the LIGO
compiler like this (the output will sort the elements of the set, but
that order is not significant for the compiler):

```shell
ligo evaluate-value
gitlab-pages/docs/language-basics/src/sets-lists-tuples/sets.religo my_set
# Outputs: { 3 ; 2 ; 1 }
```

### Set Membership



PascaLIGO features a special keyword `constains` that operates like an
infix operator checking membership in a set.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const contains_3 : bool = my_set contains 3
```
<!--CameLIGO-->
```cameligo group=a
let contains_3 : bool = Set.mem 3 my_set
```
<!--ReasonLIGO-->
```reasonligo group=a
let contains_3 : bool = Set.mem (3, my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Cardinal

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const set_size : nat = size (my_set)
```
<!--CameLIGO-->
```cameligo group=a
let set_size : nat = Set.size my_set
```
<!--ReasonLIGO-->
```reasonligo group=a
let set_size : nat = Set.size (my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Adding or Removing from a Set



In PascaLIGO, there are two ways to update a set. Either we create a
new set from the given one, or we modify it in-place. First, let us
consider the former:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const larger_set  : set (int) = set_add (4, my_set)

const smaller_set : set (int) = set_remove (3, my_set)
```
<!--END_DOCUSAURUS_CODE_TABS-->

If we are in a block, we can use an instruction to modify the set
bound to a given variable. This is called a *patch*. It is only
possible to add elements by means of a patch, not remove any: it is
the union of two sets.

In the following example, the parameter set `s` of function `update`
is augmented (as the `with s` shows) to include `4` and `7`, that is,
this instruction is equivalent to perform the union of two sets, one
that is modified in-place, and the other given as a literal
(extensional definition).

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
function update (var s : set (int)) : set (int) is block {
  patch s with set [4; 7]
} with s

const new_set : set (int) = update (my_set)
```
<!--END_DOCUSAURUS_CODE_TABS-->


In CameLIGO, we update a given set by creating another one, with or
without some elements.

<!--DOCUSAURUS_CODE_TABS-->
<!--CameLIGO-->
```cameligo group=a
let larger_set  : int set = Set.add 4 my_set

let smaller_set : int set = Set.remove 3 my_set
```
<!--END_DOCUSAURUS_CODE_TABS-->


In ReasonLIGO, we update a given set by creating another one, with or
without some elements.

<!--DOCUSAURUS_CODE_TABS-->
<!--ReasonLIGO-->
```reasonligo group=a
let larger_set  : set (int) = Set.add (4, my_set);

let smaller_set : set (int) = Set.remove (3, my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->


### Folding over a Set


Given a set, we may want to apply a function in turn to all the
elements it contains, while accumulating some value which is returned
at the end. This is a *fold operation*. In the following example, we
sum up all the elements of the set `my_set` defined above.


In PascaLIGO, the folded function takes the accumulator first and the
(current) set element second. The predefined fold is called `set_fold`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
function sum (const acc : int; const i : int): int is acc + i

const sum_of_elements : int = set_fold (sum, my_set, 0)
```
<!--END_DOCUSAURUS_CODE_TABS-->

It is possible to use a *loop* over a set as well.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
function loop (const s : set (int)) : int is block {
  var sum : int := 0;
  for element in set s block {
    sum := sum + element
  }
} with sum
```
<!--END_DOCUSAURUS_CODE_TABS-->

In CameLIGO, the predefined fold over sets is called `Set.fold`.

<!--DOCUSAURUS_CODE_TABS-->
<!--CameLIGO-->
```cameligo group=a
let sum (acc, i : int * int) : int = acc + i

let sum_of_elements : int = Set.fold sum my_set 0
```
<!--END_DOCUSAURUS_CODE_TABS-->

In ReasonLIGO, the predefined fold over sets is called `Set.fold`.

<!--DOCUSAURUS_CODE_TABS-->
<!--ReasonLIGO-->
```reasonligo group=a
let sum = ((acc, i) : (int, int)) : int => acc + i;

let sum_of_elements : int = Set.fold (sum, my_set, 0);
```
<!--END_DOCUSAURUS_CODE_TABS-->

