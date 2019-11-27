---
id: sets-lists-touples
title: Sets, Lists, Tuples
---

Apart from complex data types such as `maps` and `records`, ligo also exposes `sets`, `lists` and `tuples`.

> ⚠️ Make sure to pick the appropriate data type for your use case; it carries not only semantic but also gas related costs.

## Sets

Sets are similar to lists. The main difference is that elements of a `set` must be *unique*.

### Defining a set

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type int_set is set(int);
const my_set: int_set = set 
    1; 
    2; 
    3; 
end
```

<!--Cameligo-->
```cameligo
type int_set = int set
let my_set: int_set =
  Set.add 3 (Set.add 2 (Set.add 1 (Set.empty: int set)))
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Empty sets

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const my_set: int_set = set end;
const my_set_2: int_set = set_empty;
```
<!--Cameligo-->
```cameligo
let my_set: int_set = (Set.empty: int set)
```

<!--END_DOCUSAURUS_CODE_TABS-->

### Checking if set contains an element

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const contains_three: bool = my_set contains 3;
// or alternatively
const contains_three_fn: bool = set_mem(3, my_set);
```

<!--Cameligo-->
```cameligo
let contains_three: bool = Set.mem 3 my_set
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Obtaining the size of a set
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const set_size: nat = size(my_set);
```

<!--Cameligo-->
```cameligo
let set_size: nat = Set.size my_set
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Modifying a set
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const larger_set: int_set = set_add(4, my_set);
const smaller_set: int_set = set_remove(3, my_set);
```

<!--Cameligo-->

```cameligo
let larger_set: int_set = Set.add 4 my_set
let smaller_set: int_set = Set.remove 3 my_set
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Folding a set
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function sum(const result: int; const i: int): int is result + i;
// Outputs 6
const sum_of_a_set: int = set_fold(sum, my_set, 0);
```

<!--Cameligo-->
```cameligo
let sum (result: int) (i: int) : int = result + i
let sum_of_a_set: int = Set.fold sum my_set 0
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Lists

Lists are similar to sets, but their elements don't need to be unique and they don't offer the same range of built-in functions.

> 💡 Lists are useful when returning operations from a smart contract's entrypoint.

### Defining a list

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type int_list is list(int);
const my_list: int_list = list
    1;
    2;
    3;
end
```

<!--Cameligo-->
```cameligo
type int_list = int list
let my_list: int_list = [1; 2; 3]
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Appending an element to a list

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const larger_list: int_list = cons(4, my_list);
const even_larger_list: int_list = 5 # larger_list;
```

<!--Cameligo-->
```cameligo
let larger_list: int_list = 4 :: my_list
(* CameLIGO doesn't have a List.cons *)
```

<!--END_DOCUSAURUS_CODE_TABS-->

<br/>
> 💡 Lists can be iterated, folded or mapped to different values. You can find additional examples [here](https://gitlab.com/ligolang/ligo/tree/dev/src/test/contracts) and other built-in operators [here](https://gitlab.com/ligolang/ligo/blob/dev/src/passes/operators/operators.ml#L59)

### Mapping of a list

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function increment(const i: int): int is block { skip } with i + 1;
// Creates a new list with elements incremented by 1
const incremented_list: int_list = list_map(increment, even_larger_list);
```

<!--Cameligo-->

```cameligo
let increment (i: int) : int = i + 1
(* Creates a new list with elements incremented by 1 *)
let incremented_list: int_list = List.map increment larger_list
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Folding of a list:
<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
function sum(const result: int; const i: int): int is block { skip } with result + i;
// Outputs 6
const sum_of_a_list: int = list_fold(sum, my_list, 0);
```

<!--Cameligo-->

```cameligo
let sum (result: int) (i: int) : int = result + i
// Outputs 6
let sum_of_a_list: int = List.fold sum my_list 0
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Tuples

Tuples are used to store related data that has a **specific order** and **defined
length** without the need for named fields or a dedicated type identity. Probably
the most common tuple is a pair of type `(a, b)`. For example, if we were storing
coordinates on a two dimensional grid we might use a pair tuple of type `int * int`
to store the coordinates x and y. There is a **specific order** because x and y must
always stay in the same location within the tuple for the data to make sense. There is
also a **defined length** because the tuple pair can only ever have two elements,
if we added a third dimension `z` its type would be incompatible with that of the
pair tuple.

Like records, tuples can have members of arbitrary types in the same structure.

### Defining a tuple

Unlike [a record](language-basics/maps-records.md), tuple types do not have to be
defined before they can be used. However below we will give them names for the
sake of illustration.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type full_name is string * string;
const full_name: full_name = ("Alice", "Johnson");
```

<!--Cameligo-->
```cameligo
type full_name = string * string
(* The parenthesis here are optional *)
let full_name: full_name = ("Alice", "Johnson")
```

<!--END_DOCUSAURUS_CODE_TABS-->


### Accessing an element in a tuple

The traditional way to access the elements of a tuple in OCaml is through
[a pattern match](language-basics/unit-option-pattern-matching.md). LIGO **does
not** currently support tuple patterns in its syntaxes.

However, it is possible to access LIGO tuples by their position.
Tuple elements are one-indexed and accessed like so:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const first_name: string = full_name.1;
```

<!--Cameligo-->
```cameligo
let first_name: string = full_name.1
```

<!--END_DOCUSAURUS_CODE_TABS-->
