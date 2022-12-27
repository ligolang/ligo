---
id: list-reference
title: List
description: List operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="pascaligo">
val length : nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val length : nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let length: nat
</SyntaxTitle>

Get the number of elements in a list.

<Syntax syntax="pascaligo">

```pascaligo group=lists
const xs: list (int) = list [1; 2; 3]

const length : nat = List.length (xs)
```

</Syntax>
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

<SyntaxTitle syntax="pascaligo">
val size : nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val size : nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let size: nat
</SyntaxTitle>

Get the number of elements in a list.

Synonym for `List.length`.

<Syntax syntax="pascaligo">

```pascaligo group=lists
const size : nat = List.size (xs)
```

</Syntax>
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

<SyntaxTitle syntax="pascaligo">
val head_opt&lt;a&gt; : list (a) -> option (a)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val head_opt : 'a list -> 'a option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let head_opt : (list: list&lt;'a&gt;) => option&lt;'a&gt;
</SyntaxTitle>

Get the head of a list

<Syntax syntax="pascaligo">

```pascaligo group=lists
const head_opt : option (int) = List.head_opt (xs)
```

</Syntax>
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

<SyntaxTitle syntax="pascaligo">
val tail_opt&lt;a&gt; : list (a) -> option (list (a))
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val tail_opt : 'a list -> 'a list option
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let tail_opt : (list: list&lt;'a&gt;) => option&lt;list&lt;'a&gt;&gt;
</SyntaxTitle>

Get the tail of a list

<Syntax syntax="pascaligo">

```pascaligo group=lists
const tail_opt : option (list (int)) = List.tail_opt (xs)
```

</Syntax>
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

<SyntaxTitle syntax="pascaligo">
val iter&lt;a&gt; : (a -> unit) -> list(a) -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val iter : ('a -> unit) -> 'a list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let iter: (iterator: ((item: 'a) => unit), list: list&lt;'a&gt;) => unit
</SyntaxTitle>

Iterate over items in a list.

<Syntax syntax="pascaligo">

```pascaligo group=lists
function iter_op (const l : list (int)) : unit is {
  function iterated (const i : int) : unit is
    if i <= 3 then (failwith ("Below range.") : unit)
} with List.iter (iterated, l)
```

Alternatively it's also possible to use [loops](../language-basics/loops.md).

</Syntax>
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


<SyntaxTitle syntax="pascaligo">
val map&lt;a,b&gt; : (a -> b) -> list (a) -> list (b)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val map : ('a -> 'b) -> 'a list -> 'b list
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let map: (mapper: ((item: 'a) => 'b), list: list&lt;'a&gt;) => list&lt;'b&gt;
</SyntaxTitle>

Apply a function to items of a list to create a new list.

<Syntax syntax="pascaligo">

```pascaligo group=lists
const larger_list: list (int) = list [1; 2; 3]

function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = List.map (increment, larger_list)
```

</Syntax>
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

<SyntaxTitle syntax="pascaligo">
val fold&lt;item,acc&gt : (acc * item -> acc) * list (item) * acc -> acc
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold : ('acc * 'item -> 'acc) -> 'item list -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold: ((folder: [acc: &apos;acc, item: &apos;item]) => &apos;acc, list: list&lt;&apos;item&gt;, acc: &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="pascaligo">

```pascaligo group=lists
const my_list : list (int) = list [1; 2; 3]

function sum (const acc : int; const i : int): int is acc + i

const sum_of_elements : int = List.fold (sum, my_list, 0)
```

</Syntax>
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

let sum_fold = (result: int, i: int): int => result + i;

let sum_of_elements_fold: int = List.fold(sum_fold, my_list_fold, 0);
```

</Syntax>
<SyntaxTitle syntax="pascaligo">
val fold_left&lt;item,acc&gt; : (acc * item -> acc) * acc * list (item) -> acc
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold_left : ('acc * 'item -> 'acc) -> 'acc -> 'item list -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold_left: (((a: [&apos;acc, &apos;item]) => &apos;acc), &apos;acc, list&lt;&apos;item&gt;) => &apos;acc
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="pascaligo">

```pascaligo group=lists
const my_list : list (int) = list [1; 2; 3]

function sum (const acc : int; const i : int) : int is acc + i

const sum_of_elements : int = List.fold_left (sum, 0, my_list)
```

</Syntax>
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

let sum = (result: int, i: int): int => result + i;

let sum_of_elements : int = List.fold_left (sum, 0, my_list);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val fold_right&lt;item,acc&gt; : (item * acc -> acc) * list (item) * acc -> acc
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold_right : ('item * 'acc -> 'acc) -> 'item list -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold_right: (((a: [&apos;item, &apos;acc]) => &apos;acc), list&lt;&apos;item&gt;, &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over items in a list](../language-basics/sets-lists-tuples.md#folded-operation-over-lists);

<Syntax syntax="pascaligo">

```pascaligo group=lists
const my_list : list (int) = list [1; 2; 3]

function sum_right (const i : int; const acc : int) : int is acc + i

const sum_of_elements : int = List.fold_right (sum_right, my_list, 0)
```

</Syntax>
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

let sum_right = (i: int, result: int): int => result + i;

let sum_of_elements : int = List.fold_right (sum_right, my_list, 0);
```

</Syntax>
