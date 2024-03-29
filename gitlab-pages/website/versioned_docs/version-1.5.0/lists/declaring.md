---
id: declaring
title: Declaring
---

import Syntax from '@theme/Syntax';

Lists are linear collections of elements of the same type. Linear
means that, in order to reach an element in a list, we must visit all
the elements before (sequential access). Elements can be repeated, as
only their order in the collection matters. The first element is
called the *head*, and the sub-list after the head is called the
*tail*. For those familiar with algorithmic data structure, you can
think of a list a *stack*, where the top is written on the left.

> 💡 Lists are needed when returning operations from a smart
> contract.

The type for lists is polymorphic, that is, parameterised by the type
of the list elements, so we can define a "list of integers", a "list
of natural numbers" etc.

<Syntax syntax="cameligo">

```cameligo group=lists
let empty_list : int list = []
let my_list : int list = [1; 2; 2] (* The head is 1, the tail is [2; 2] *)
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
const empty_list: list<int> = list([]);
const my_list = list([1, 2, 2]); // The head is 1, the tail is list([2, 2])
```

Note how we need to use the cast `list(...)` on a tuple to make it a
list. In general, tuples are not lists: tuples have a fixed number of
components that appear in their type, and each component can have a
different type, whereas lists have a variable number of elements and
they have all the same type. Nevertheless, LIGO uses the same syntax
for tuples and lists, except that the latter is enclosed in
`list(...)`, except when the context makes it unambiguous that it is a
list (we will see some example with pattern matching).

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
