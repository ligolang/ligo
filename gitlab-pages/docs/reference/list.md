---
id: list-reference
title: List — Ordered collection of a type
---

## List.size(lst: a' list) : nat

Get the number of elements in a list.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo
function size_ (const m : list(int)) : nat is size(m)
```

<!--CameLIGO-->
```cameligo
let size_ (s: int list) : nat = List.size s
```

<!--ReasonLIGO-->
```reasonligo
let size_ = (s: list(int)): nat => List.size(s);
```

<!--END_DOCUSAURUS_CODE_TABS-->

## List.length(lst: a' list) : nat

Alias of `List.size`.

## List.map(map_function: a' -> b', lst: a' list) : 'b list

Apply an operation defined by `map_function` to each element of a list and return
a list of the modified elements.

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
