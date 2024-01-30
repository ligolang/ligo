---
id: set-reference
title: Set
description: Set operations
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

Sets are unordered collections of unique values of the same type.

<SyntaxTitle syntax="cameligo">
val empty : 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let empty: set&lt;&apos;elt&gt;
</SyntaxTitle>

Create an empty set.

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

<SyntaxTitle syntax="cameligo">
val literal : 'elt list -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let literal: (input: list&lt;&apos;elt&gt;) => set&lt;&apos;elt&gt;
</SyntaxTitle>

Create a non-empty set.

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

<SyntaxTitle syntax="cameligo">
val mem : 'elt -> 'elt set -> bool
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let mem: (v: &apos;elt, set: set&lt;&apos;elt&gt;) => bool
</SyntaxTitle>

Checks if a value exists in the set.

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

<SyntaxTitle syntax="cameligo">
val cardinal : 'elt set -> nat
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let cardinal: (set: set&lt;&apos;elt&gt;) => nat
</SyntaxTitle>

Number of elements in a set.

Notice: Previously, `Set.size` was used for the number of elements in
a set. `Set.size` is now marked for deprecation, and `Set.cardinal`
should be used instead.

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

<SyntaxTitle syntax="cameligo">
val add : 'elt -> 'elt set -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let add: (elt: &apos;elt, set: set&lt;&apos;elt&gt;) => set&lt;&apos;elt&gt;
</SyntaxTitle>

Add a value to a set.

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

<SyntaxTitle syntax="cameligo">
val remove : 'elt -> 'elt set -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let remove: (elt: &apos;elt, set: set&lt;&apos;elt&gt;) => set&lt;&apos;elt&gt;
</SyntaxTitle>

Remove a value from a set.

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

<SyntaxTitle syntax="cameligo">
val update : 'elt -> bool -> 'elt set -> 'elt set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let update: (elt : 'elt, flag : bool, set : set&lt;'elt&gt;) => set&lt;'elt&gt;
</SyntaxTitle>

Add or remove an element in a set based on the boolean value being passed.

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

<SyntaxTitle syntax="cameligo">
val iter : ('elt -> unit) -> 'elt set -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let iter: (iterator: ((item: &apos;a) => unit), set: set&lt;&apos;a&gt;) => unit
</SyntaxTitle>

Iterate over values in a set.

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

<SyntaxTitle syntax="cameligo">
val fold : ('acc * 'item -> 'acc) -> 'item set -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold: ((iterator: [acc: &apos;acc, item: &apos;item]) => &apos;acc), set: set&lt;&apos;item&gt;, acc: &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over values in a set](../language-basics/sets-lists-tuples.md#folded-operation)


<Syntax syntax="cameligo">

```cameligo group=sets
let sum (acc, i : int * int) : int = acc + i
let sum_of_elements : int = Set.fold sum my_set 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
let sum = ([acc, i]: [int, int]): int => acc + i;
let sum_of_elements: int = Set.fold(sum, my_set, 0);
```

</Syntax>

<SyntaxTitle syntax="cameligo">
val fold_desc : (('item * 'acc) -> 'acc) -> 'item set -> 'acc -> 'acc
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let fold_desc: (((a: [&apos;item, &apos;acc]) => &apos;acc), set&lt;&apos;item&gt;, &apos;acc) => &apos;acc
</SyntaxTitle>

[Fold over values in a set](../language-basics/sets-lists-tuples.md#folded-operation)


<Syntax syntax="cameligo">

```cameligo group=sets
let sum_right (i, acc : int * int) : int = acc + i
let sum_of_elements : int = Set.fold_desc sum_right my_set 0
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=sets
let sum_right = ([i, acc]: [int, int]) : int => acc + i;
let sum_of_elements_desc : int = Set.fold_desc (sum_right, my_set, 0);
```

</Syntax>


<SyntaxTitle syntax="cameligo">
val filter_map : ('a -> 'b option) -> 'a set -> 'b set
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let filter_map: (mapper: ((item: 'a) => option&lt;'b&gt;), set: set&lt;'a&gt;) => set&lt;'b&gt;
</SyntaxTitle>

Apply a function to items of a set to create a new set, but the function can omit certain elements by returning `None`.

Notice: built in terms of `fold_desc`.
