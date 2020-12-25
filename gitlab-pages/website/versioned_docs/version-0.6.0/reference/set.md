---
id: set-reference
title: Set
description: Set operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

Sets are unordered collections of unique values of the same type.

<SyntaxTitle syntax="pascaligo">
function empty : set('value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val empty : 'value set
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let empty: set('value)
</SyntaxTitle>

Create an empty set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const my_set : set (int) = Set.empty
```

Alternative syntax:

```pascaligo group=sets
const my_set : set (int) = set []
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let my_set : int set = Set.empty
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sets
let my_set : set (int) = Set.empty;
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function literal : list('value) -> set('value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val literal : 'value list -> 'value set
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let literal: list('value) => set('value)
</SyntaxTitle>

Create a non-empty set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const my_set : set (int) = Set.literal (list [3; 2; 2; 1])
```

Or use the following syntax sugar:

```pascaligo group=sets
const my_set : set (int) = set [3; 2; 2; 1]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let my_set : int set =
  Set.literal [3; 2; 2; 1]
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sets
let my_set : set (int) =
  Set.literal ([3, 2, 2, 1]);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function mem : 'value -> set('value) -> 'bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val mem : 'value -> 'value set -> bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let mem: ('value, set('value)) => bool
</SyntaxTitle>

Checks if a value exists in the set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const contains_3 : bool = Set.mem(3, my_set)
```

Or:

```pascaligo group=sets
const contains_3_alt : bool = my_set contains 3
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let contains_3 : bool = Set.mem 3 my_set
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo group=sets
let contains_3 : bool = Set.mem (3, my_set);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
function cardinal : set('value) -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val cardinal : 'value set -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let cardinal: set('value) => nat
</SyntaxTitle>

Number of elements in a set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const cardinal : nat = Set.size (my_set)
```

> Note that `size` is *deprecated*. Please use `Set.size`

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

<SyntaxTitle syntax="pascaligo">
function add : 'value -> set('value) -> set('value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val add : 'value -> 'value set -> 'value set
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let add: ('value, set('value)) => set('value)
</SyntaxTitle>

Add a value to a set.

<SyntaxTitle syntax="pascaligo">
function remove : 'value -> set('value) -> set('value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val remove : 'value -> 'value set -> 'value set
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let remove: ('value, set('value)) => set('value)
</SyntaxTitle>

Remove a value from a set.

<SyntaxTitle syntax="pascaligo">
function iter : ('a -> unit) -> set('a) -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val iter : ('a -> unit) -> 'a set -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let iter: (('a => unit), set('a)) => unit
</SyntaxTitle>

Iterate over values in a set.


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

<SyntaxTitle syntax="pascaligo">
function fold : (('accumulator -> 'item -> 'accumulator) -> set ('item) -> 'accumulator) -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold : ('accumulator -> 'item -> 'accumulator) -> 'set list -> 'accumulator -> 'accumulator
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let fold: ((('accumulator, 'item) => 'accumulator), set('item), 'accumulator) => 'accumulator
</SyntaxTitle>

[Fold over values in a set](../language-basics/sets-lists-tuples.md#folded-operation)


<Syntax syntax="pascaligo">

```pascaligo group=sets
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = Set.fold (sum, my_set, 0)
```

> Note that `set_fold` is *deprecated*.

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
