---
id: list-reference
title: List
description: List operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val length : nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let length: nat
</SyntaxTitle>

Get the number of elements in a list.

<Syntax syntax="cameligo">

```cameligo group=lists
let xs : int list = [1; 2; 3]

let length : nat = List.length xs
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let xs : list<int> = list([1, 2, 3]);

let length : nat = List.length (xs);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val size : nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let size: nat
</SyntaxTitle>

Get the number of elements in a list.

Synonym for `List.length`.

<Syntax syntax="cameligo">

```cameligo group=lists
let size : nat = List.size xs
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let size : nat = List.size (xs);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val head_opt : 'a list -> 'a option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let head_opt : (list: list&lt;'a&gt;) => option&lt;'a&gt;
</SyntaxTitle>

Get the head of a list

<Syntax syntax="cameligo">

```cameligo group=lists
let head_opt : int option = List.head_opt xs
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let head_opt : option<int>  = List.head_opt (xs);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val tail_opt : 'a list -> 'a list option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let tail_opt : (list: list&lt;'a&gt;) => option&lt;list&lt;'a&gt;&gt;
</SyntaxTitle>

Get the tail of a list

<Syntax syntax="cameligo">

```cameligo group=lists
let tail_opt : int list option = List.tail_opt xs
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let tail_opt : option<list<int>> = List.tail_opt (xs);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val iter : ('a -> unit) -> 'a list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let iter: (iterator: ((item: 'a) => unit), list: list&lt;'a&gt;) => unit
</SyntaxTitle>

Iterate over items in a list.

<Syntax syntax="cameligo">

```cameligo group=lists
let iter_op (l : int list) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in List.iter predicate l
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let iter_op = (l: list<int>): unit => {
  let predicate = (i: int): unit => assert(i > 3);
  List.iter(predicate, l);
};
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a list -> 'b list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let map: (mapper: ((item: 'a) => 'b), list: list&lt;'a&gt;) => list&lt;'b&gt;
</SyntaxTitle>

Apply a function to items of a list to create a new list.

<Syntax syntax="cameligo">

```cameligo group=lists
let larger_list: int list = [1; 2; 3]

let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let larger_list: list<int> = list([1, 2, 3]);

let increment = (i : int): int => i + 1;

// Creates a new list with all elements incremented by 1
let plus_one : list<int> = List.map(increment, larger_list);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val fold : ('acc * 'item -> 'acc) -> 'item list -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold: ((folder: [acc: &apos;acc, item: &apos;item]) => &apos;acc, list: list&lt;&apos;item&gt;, acc: &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="cameligo">

```cameligo group=lists
let my_list : int list = [1; 2; 3]

let sum (acc, i : int * int) : int = acc + i

let sum_of_elements : int = List.fold sum my_list 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists2
let my_list_fold: list<int> = list([1, 2, 3]);

let sum_fold = ([result, i]: [int, int]): int => result + i;

let sum_of_elements_fold: int = List.fold(sum_fold, my_list_fold, 0);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val fold_left : ('acc * 'item -> 'acc) -> 'acc -> 'item list -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold_left: (((a: [&apos;acc, &apos;item]) => &apos;acc), &apos;acc, list&lt;&apos;item&gt;) => &apos;acc
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="cameligo">

```cameligo group=lists
let my_list : int list = [1; 2; 3]

let sum (acc, i : int * int) : int = acc + i

let sum_of_elements : int = List.fold_left sum 0 my_list
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists3
let my_list : list<int> = list([1, 2, 3]);

let sum = ([result, i]: [int, int]): int => result + i;

let sum_of_elements : int = List.fold_left (sum, 0, my_list);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val fold_right : ('item * 'acc -> 'acc) -> 'item list -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold_right: (((a: [&apos;item, &apos;acc]) => &apos;acc), list&lt;&apos;item&gt;, &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="cameligo">

```cameligo group=lists
let my_list : int list = [1; 2; 3]

let sum_right (i, acc : int * int) : int = acc + i

let sum_of_elements : int = List.fold_right sum_right my_list 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
let my_list : list<int> = list([1, 2, 3]);

let sum_right = ([i, result]: [int, int]): int => result + i;

let sum_of_elements : int = List.fold_right (sum_right, my_list, 0);
```

</Syntax>


<SyntaxTitle syntax="cameligo">
val find_opt : ('a -> bool) -> 'a list -> 'a option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let find_opt: (mapper: ((item: 'a) => bool), list: list&lt;'a&gt;) => option&lt;'a&gt;
</SyntaxTitle>

Finds the first element satisfying the given predicate.


<SyntaxTitle syntax="cameligo">
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let filter_map: (mapper: ((item: 'a) => option&lt;'b&gt;), list: list&lt;'a&gt;) => list&lt;'b&gt;
</SyntaxTitle>

Apply a function to items of a list to create a new list, but the function can omit certain elements by returning `None`.

Notice: built in terms of `fold_right`.


<SyntaxTitle syntax="cameligo">
val update : ('a -> 'a option) -> 'a list -> 'a list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let update: (upd: ((item: 'a) => option&lt;'a&gt;), list: list&lt;'a&gt;) => list&lt;'a&gt;
</SyntaxTitle>

Apply a function to items of a list to create a new list of the same type, but certain elements can be changed using `upd`.

Notice: built in terms of `map`.


<SyntaxTitle syntax="cameligo">
val update_with : ('a -> bool) -> 'a -> 'a list -> 'a list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let update_with: (upd: ((item: 'a) => bool), new_item: 'a, list: list&lt;'a&gt;) => list&lt;'a&gt;
</SyntaxTitle>

Create a new list of the same type: if the predicate is satisfied on some element, this element is replaced for the new item.

Notice: built in terms of `map`.