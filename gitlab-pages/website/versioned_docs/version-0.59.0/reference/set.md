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
val empty&lt;elt&gt; : set (elt)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val empty : 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let empty: set&lt;&apos;elt&gt;
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

<Syntax syntax="jsligo">

```jsligo group=sets
let my_set: set<int> = Set.empty;
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val literal&lt;elt&gt; : list (elt) -> set (elt)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val literal : 'elt list -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let literal: (input: list&lt;&apos;elt&gt;) => set&lt;&apos;elt&gt;
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
let my_set : int set = Set.literal [3; 2; 2; 1]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets2
let my_set : set<int> = Set.literal(list([3, 2, 2, 1]));
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val mem&lt;elt&gt; : elt * set (elt) -> bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val mem : 'elt -> 'elt set -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let mem: (v: &apos;elt, set: set&lt;&apos;elt&gt;) => bool
</SyntaxTitle>

Checks if a value exists in the set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const contains_3 : bool = Set.mem (3, my_set)
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

<Syntax syntax="jsligo">

```jsligo group=sets2
let contains_3 : bool = Set.mem (3, my_set);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val cardinal&lt;elt&gt; : set (elt) -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val cardinal : 'elt set -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let cardinal: (set: set&lt;&apos;elt&gt;) => nat
</SyntaxTitle>

Number of elements in a set.

Notice: previously, `Set.size` was used for the number of elements in
a set. `Set.size` is now marked for deprecation, and `Set.cardinal`
should be used instead.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const card : nat = Set.cardinal (my_set)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let card : nat = Set.cardinal my_set
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
let card: nat = Set.cardinal(my_set);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val add&lt;elt&gt; : elt * set (elt) -> set(elt)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val add : 'elt -> 'elt set -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let add: (elt: &apos;elt, set: set&lt;&apos;elt&gt;) => set&lt;&apos;elt&gt;
</SyntaxTitle>

Add a value to a set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const updated_set = Set.add (4, my_set)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let updated_set = Set.add 4 my_set
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
let updated_set = Set.add (4, my_set);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val remove&lt;elt&gt; : elt * set (elt) -> set (elt)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val remove : 'elt -> 'elt set -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let remove: (elt: &apos;elt, set: set&lt;&apos;elt&gt;) => set&lt;&apos;elt&gt;
</SyntaxTitle>

Remove a value from a set.

<Syntax syntax="pascaligo">

```pascaligo group=sets
const updated_set = Set.remove (3, my_set)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let updated_set = Set.remove 3 my_set
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets2
let updated_set = Set.remove (3, my_set);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val update&lt;elt&gt; : elt * bool * set (elt) -> set (elt)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val update : 'elt -> bool -> 'elt set -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let update: (elt : 'elt, flag : bool, set : set&lt;'elt&gt;) => set&lt;'elt&gt;
</SyntaxTitle>


add or remove an element in a set based on the boolean value being passed.

<Syntax syntax="pascaligo">

```pascaligo group=sets
// in case of True value will be added to the set
const updated_set = Set.update (4, True, my_set)

// in case of False value will be removed from the set
const updated_set = Set.update (4, False, my_set)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
(* in case of true value will be added to the set *)
let updated_set = Set.update 4 true my_set

(* in case of false value will be removed from the set *)
let updated_set = Set.update 4 false my_set
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
// in case of true value will be added to the set
let updated_set2 = Set.update (4, true, my_set);

// in case of false value will be removed from the set
let updated_set3 = Set.update (4, false, my_set);
```

</Syntax>


<SyntaxTitle syntax="pascaligo">
val iter&lt;elt&gt; : (elt -> unit) * set (elt) -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val iter : ('elt -> unit) -> 'elt set -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let iter: (iterator: ((item: &apos;a) => unit), set: set&lt;&apos;a&gt;) => unit
</SyntaxTitle>

Iterate over values in a set.



<Syntax syntax="pascaligo">

```pascaligo group=sets
function iter_op (const s : set (int)) : unit is {
  function iterated (const i : int) : unit is
    if i <= 2 then (failwith ("Below range.") : unit)
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

<Syntax syntax="jsligo">

```jsligo group=sets
let iter_op = (s: set<int>): unit => {
  let predicate = (i : int): unit => assert(i > 3);
  Set.iter(predicate, s);
};
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
val fold&lt;item,acc&gt; : ((acc -> item -> acc) * set (item) * acc) -> acc
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold : ('acc * 'item -> 'acc) -> 'item set -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold: ((iterator: [acc: &apos;acc, item: &apos;item]) => &apos;acc), set: set&lt;&apos;item&gt;, acc: &apos;acc) => &apos;acc
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

<Syntax syntax="jsligo">

```jsligo group=sets
let sum = (acc: int, i: int): int => acc + i;
let sum_of_elements: int = Set.fold(sum, my_set, 0);
```

</Syntax>
<SyntaxTitle syntax="pascaligo">
val fold_desc&lt;item,acc&gt; : (item * acc -> acc) * set (item) * acc -> acc
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val fold_desc : (('item * 'acc) -> 'acc) -> 'item set -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold_desc: (((a: [&apos;item, &apos;acc]) => &apos;acc), set&lt;&apos;item&gt;, &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over values in a set](../language-basics/sets-lists-tuples.md#folded-operation)


<Syntax syntax="pascaligo">

```pascaligo group=sets
function sum_right (const i : int; const acc : int) : int is acc + i
const sum_of_elements : int = Set.fold_desc (sum_right, my_set, 0)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo group=sets
let sum_right (i, acc : int * int) : int = acc + i
let sum_of_elements : int = Set.fold_desc sum_right my_set 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
let sum_right = (i: int, acc: int) : int => acc + i;
let sum_of_elements_desc : int = Set.fold_desc (sum_right, my_set, 0);
```

</Syntax>
