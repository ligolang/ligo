---
id: set-reference
title: Set
---

## Defining a set

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
type int_set is set (int);
const my_set : int_set = set 1; 2; 3 end
```

<!--CameLIGO-->
```cameligo group=a
type int_set = int set
let my_set : int_set =
  Set.add 3 (Set.add 2 (Set.add 1 (Set.empty: int set)))
```

<!--ReasonLIGO-->
```reasonligo group=a
type int_set = set (int);
let my_set : int_set =
  Set.add (3, Set.add (2, Set.add (1, Set.empty: set (int))));
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Set.mem(is_member: a', s: a' set) : bool

Check if a set `s` contains the element `is_member`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const contains_three : bool = my_set contains 3
// or alternatively
const contains_three_fn: bool = set_mem (3, my_set);
```

<!--CameLIGO-->
```cameligo group=a
let contains_three: bool = Set.mem 3 my_set
```
<!--ReasonLIGO-->
```reasonligo group=a
let contains_three: bool = Set.mem(3, my_set);
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Set.empty() : a' set

Create a new empty set. Needs to be annotated with the set type.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const my_set: int_set = set end
const my_set_2: int_set = set_empty
```
<!--CameLIGO-->
```cameligo group=a
let my_set: int_set = (Set.empty: int set)
```
<!--ReasonLIGO-->
```reasonligo group=a
let my_set: int_set = (Set.empty: set (int));
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Set.literal(element_list_literal: 'a list) : 'a set

Create a set from the elements of a list. Note that **you must pass a list literal**
to this function, a variable will not work.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
const s_fb : set(string) = set [
      "foo" ;
      "bar" ;
]
```

<!--CameLIGO-->
```cameligo
let literal_op (p: unit) : string set =
  Set.literal ["foo"; "bar"; "foobar"]
```

<!--ReasonLIGO-->
```reasonligo
let literal_op = (p: unit) : set(string) => Set.literal(["foo", "bar", "foobar"]);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Set.add(addition: a', s: a' set) : a' set

Add the element `addition` to a set `s`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
function add_op (const s : set(string)) : set(string) is
  begin skip end with set_add("foobar" , s)
```

<!--CameLIGO-->
```cameligo group=a
type int_set = int set
let my_set : int_set =
  Set.add 3 (Set.add 2 (Set.add 1 (Set.empty: int set)))
```

<!--ReasonLIGO-->
```reasonligo group=a
type int_set = set (int);
let my_set : int_set =
  Set.add (3, Set.add (2, Set.add (1, Set.empty: set (int))));
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Set.remove(removal: a', s: a' set) : a' set

Remove the element `removal` from a set `s`.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const smaller_set: int_set = set_remove(3, my_set);
```

<!--CameLIGO-->

```cameligo group=a
let smaller_set: int_set = Set.remove 3 my_set
```

<!--ReasonLIGO-->

```reasonligo group=a
let smaller_set: int_set = Set.remove(3, my_set);
```

<!--END_DOCUSAURUS_CODE_TABS-->


## Set.fold(folding_function: a' -> a' -> a', s: a' set, initial: a') : a'

Combine the elements of a set into a single value using a folding function.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
function sum(const result: int; const i: int): int is result + i;
// Outputs 6
const sum_of_a_set: int = set_fold(sum, my_set, 0);
```

<!--CameLIGO-->
```cameligo group=a
let sum (result, i: int * int) : int = result + i
let sum_of_a_set: int = Set.fold sum my_set 0
```

<!--ReasonLIGO-->
```reasonligo group=a
let sum = (result_i: (int, int)): int => result_i[0] + result_i[1];
let sum_of_a_set: int = Set.fold(sum, my_set, 0);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Set.size(s: a' set) : nat

Get the number of elements in a set.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo group=a
const set_size: nat = size (my_set)
```

<!--CameLIGO-->
```cameligo group=a
let set_size: nat = Set.size my_set
```

<!--ReasonLIGO-->
```reasonligo group=a
let set_size: nat = Set.size (my_set);
```

<!--END_DOCUSAURUS_CODE_TABS-->
