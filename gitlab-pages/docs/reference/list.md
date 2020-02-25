---
id: list-reference
title: List — Linear Collections
---

Lists are linear collections of elements of the same type. Linear
means that, in order to reach an element in a list, we must visit all
the elements before (sequential access). Elements can be repeated, as
only their order in the collection matters. The first element is
called the *head*, and the sub-list after the head is called the
*tail*. For those familiar with algorithmic data structure, you can
think of a list a *stack*, where the top is written on the left.

## Defining Lists

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=lists
const empty_list : list (int) = nil // Or list []
const my_list : list (int) = list [1; 2; 2] // The head is 1
```

<!--CameLIGO-->
```cameligo group=lists
let empty_list : int list = []
let my_list : int list = [1; 2; 2] // The head is 1
```

<!--ReasonLIGO-->
```reasonligo group=lists
let empty_list : list (int) = [];
let my_list : list (int) = [1, 2, 2]; // The head is 1
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Adding to Lists

Lists can be augmented by adding an element before the head (or, in
terms of stack, by *pushing an element on top*).

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=lists
const larger_list : list (int) = 5 # my_list // [5;1;2;2]
```

<!--CameLIGO-->

```cameligo group=lists
let larger_list : int list = 5 :: my_list // [5;1;2;2]
```

<!--ReasonLIGO-->

```reasonligo group=lists
let larger_list : list (int) = [5, ...my_list]; // [5,1,2,2]
```
<!--END_DOCUSAURUS_CODE_TABS-->


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

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=lists
function iter_op (const l : list (int)) : unit is
  block {
    function iterated (const i : int) : unit is
      if i > 2 then Unit else (failwith ("Below range.") : unit)
  } with list_iter (iterated, l)
```
<!--CameLIGO-->

```cameligo group=lists
let iter_op (l : int list) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in List.iter predicate l
```

<!--ReasonLIGO-->

```reasonligo group=lists
let iter_op = (l : list (int)) : unit => {
  let predicate = (i : int) => assert (i > 3);
  List.iter (predicate, l);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

#### Mapped Operation over Lists

We may want to change all the elements of a given list by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=lists
function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = list_map (increment, larger_list)
```

<!--CameLIGO-->

```cameligo group=lists
let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```

<!--ReasonLIGO-->

```reasonligo group=lists
let increment = (i : int) : int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list (int) = List.map (increment, larger_list);
```
<!--END_DOCUSAURUS_CODE_TABS-->

#### Folded Operation over Lists

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

```pascaligo group=lists
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = list_fold (sum, my_list, 0)
```

<!--CameLIGO-->

```cameligo group=lists
let sum (acc, i: int * int) : int = acc + i
let sum_of_elements : int = List.fold sum my_list 0
```

<!--ReasonLIGO-->

```reasonligo group=lists
let sum = ((result, i): (int, int)): int => result + i;
let sum_of_elements : int = List.fold (sum, my_list, 0);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## List Length

Get the number of elements in a list.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function size_of (const l : list (int)) : nat is List.length (l)
```

<!--CameLIGO-->
```cameligo
let size_of (l : int list) : nat = List.length l
```

<!--ReasonLIGO-->
```reasonligo
let size_of = (l : list (int)) : nat => List.length (l);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## List.map(map_function: a' -> b', lst: a' list) : 'b list

Apply an operation defined by `map_function` to each element of a list
and return a list of the modified elements.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
function increment(const i: int): int is i + 1;
// Creates a new list with elements incremented by 1
const incremented_list: list(int) = list_map(increment, list 1; 2; 3; end );
```

<!--CameLIGO-->

```cameligo group=b
let increment (i: int) : int = i + 1
(* Creates a new list with elements incremented by 1 *)
let incremented_list: int list = List.map increment [1; 2; 3]
```


<!--ReasonLIGO-->

```reasonligo group=b
let increment = (i: int): int => i + 1;
(* Creates a new list with elements incremented by 1 *)
let incremented_list: list(int) = List.map(increment, [1, 2, 3]);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## List.iter(iter_function: a' -> unit, lst: a' list) : unit

Apply a side effecting function `iter_function` to each element of a list with no
return value. This is useful for asserting that each element of a list satisfies
a particular property.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function iter_op (const s : list(int)) : int is
  begin
    var r : int := 0 ;
    function aggregate (const i : int) : unit is
      begin
        r := r + i ;
      end with unit ;
    list_iter(aggregate, s) ;
  end with r
```

<!--CameLIGO-->
```cameligo
let iter_op (s : int list) : unit =
  let do_nothing = fun (_: int) -> unit
  in List.iter do_nothing s
```

<!--ReasonLIGO-->
```reasonligo
let iter_op = (s: list(int)): unit => {
  let do_nothing = (z: int) => unit;
  List.iter(do_nothing, s);
};
```

<!--END_DOCUSAURUS_CODE_TABS-->

## List.fold(fold_function: (a' * a') -> a', lst: a' list, acc: a') : 'a

Combine the elements of a list into one value using the operation defined by
`fold_function'. For example, you could define summation by folding a list of
integers. Starting with some initial accumulator value `acc`, the fold:

1. Consumes an element of the list.
2. Passes the accumulator value to `fold_function` along with the element to produce
a new accumulated value.
3. The new accumulated value replaces the previous one.
4. IF there are still elements in the list go back to 1, ELSE return the accumulator

Summation would be defined then by using a `fold_function` that takes two integers and
adds them together. Each step of the fold would consume an element from the list
and add it to the total until you've summed over the list.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=b
function sum(const result: int; const i: int): int is result + i;
const sum_of_a_list: int = list_fold(sum, list 1; 2; 3; end, 0);
```

<!--CameLIGO-->

```cameligo group=b
let sum (result, i: int * int) : int = result + i
let sum_of_a_list: int = List.fold sum [1; 2; 3] 0
```

<!--ReasonLIGO-->

```reasonligo group=b
let sum = ((result, i): (int, int)): int => result + i;
let sum_of_a_list: int = List.fold(sum, [1, 2, 3], 0);
```

<!--END_DOCUSAURUS_CODE_TABS-->
