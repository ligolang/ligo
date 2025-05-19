---
title: Lists
---

import Syntax from '@theme/Syntax';

Lists are linear collections of elements of the same type.
Elements can appear more than once in a list, because only their order in the collection matters.
Lists can be empty or contain any number of elements.

In this context, _linear_ means that you cannot access elements in random order.
To access a specific element, you must visit the elements sequentially.
The first element is called the *head*, and the sub-list after the head is called the *tail*. For those familiar with algorithmic data structure, you can think of a list a *stack*, where the top is written on the left.

:::note

One important use of lists is in the return values of contract entrypoints.
Each entrypoint returns a tuple, of which the first component is a list of operations to run.

:::

Lists are similar to [sets](./sets) because they are both collections of elements of the same type.
The main differences between lists and sets are:

- Sets cannot contain duplicate entries, while lists can
- Sets are always automatically sorted, while you can put the elements of a list in any order and LIGO retains that order

The type for lists is polymorphic, that is, parameterised by the type of the list elements, so to define a list you must specify the type of the elements.
Developers say that they are defining a "list of integers", a "list of natural numbers," and so on for lists of other types.
This example shows how to define lists of integers, but you can create lists of any type as long as all of the elements have the same type:

<Syntax syntax="cameligo">

```cameligo group=lists
let empty_list : int list = []
let my_list : int list = [1; 2; 2] (* The head is 1, the tail is [2; 2] *)
```

For functions that work with lists, see the predefined [module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=lists
const empty_list: list<int> = [];
const my_list: list<int> = [1, 2, 2]; // The head is 1, the tail is [2, 2]
```

The syntax for a literal list value is the same as for a tuple.
Therefore, in some cases, you may need to use the `list` function to cast a value to make it a list instead of a tuple.
In the previous example, the `my_list` variable is a list because of the type annotation.
However, if you assign a literal list value to a variable when the context is ambiguous, you must cast it with the `list` function or else LIGO assumes that it is a tuple, as in this example:

```jsligo group=lists
const my_literal_tuple = [1, 2, 3]; // Assumed to be a tuple
const my_literal_list = list([1, 2, 3]); // Casted to a list
```

For functions that work with lists, see the predefined [namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>

:::tip

Because lists can contain any type of element, it can be convenient to use polymorphism to create functions that can operate on lists of any type.
For more information, see [Polymorphic functions](./parametric_types#polymorphic-functions).

:::

## Accessing elements

You cannot access elements directly in lists, such as getting the element with an arbitrary index.
One common way to access list elements is to access the first element (the head) or the rest of the elements (the tail) separately.
LIGO provides the functions `List.head` and `List.tail` to access list elements as options, as in these examples:

<Syntax syntax="cameligo">

```cameligo group=access
let my_list : int list = [1; 2; 3]
let head_option : int option = List.head my_list
let head = match head_option with
| Some value -> value
| None -> failwith "Failed to get the head of the list"
let tail_option : int list option = List.tail my_list
let tail = match tail_option with
| Some value -> value
| None -> failwith "Failed to get the tail of the list"
```


</Syntax>

<Syntax syntax="jsligo">

```jsligo group=access
const my_list: list<int> = [1, 2, 3];
const head_option: option<int> = List.head(my_list);
const head = $match(head_option, {
  "Some": value => value,
  "None": () => failwith("Failed to get the head of the list"),
});
const tail_option: option<list<int>> = List.tail(my_list);
const tail = $match(tail_option, {
  "Some": value => value,
  "None": () => failwith("Failed to get the tail of the list"),
});
```

:::note

In JsLIGO, you cannot access an arbitrary element in a list by its index, as in `my_list[4]`.

:::

</Syntax>

Another way to access elements in lists is to process every element in the list, as described in [Processing lists](#processing-lists).

## Adding elements

You can add an element to the beginning of a list to become the new head (or, in terms of a stack, by *pushing an element on top*).
This operation is usually called *consing* in functional languages.

<Syntax syntax="cameligo">

The *cons operator* is infix and noted "`::`". It is not symmetric: on
the left lies the element to cons, and, on the right, a list on which
to cons.

```cameligo group=consing
let short_list = [1; 2; 2]
// long_list = [5; 1; 2; 2]
let long_list : int list = 5 :: short_list
```

There is also a predefined function `List.cons`:

```cameligo group=consing
// longer_list = [6; 5; 1; 2; 2]
let longer_list = List.cons 6 long_list
```

</Syntax>

<Syntax syntax="jsligo">

The *cons operator* is infix and noted "`, ...`". It is not symmetric:
on the left lies the element to cons, and, on the right, a list on
which to cons.

```jsligo group=consing
const short_list: list<int> = [1, 2, 2];
// long_list == [5,1,2,2]
const long_list: list<int> = [5, ...short_list];
```

There is also a predefined function `List.cons`:

```jsligo group=consing
// longer_list == [6, 5, 1, 2, 2]
const longer_list = List.cons(6, long_list);
```

</Syntax>

## Removing elements

There is no direct way to remove a specific element from a list.
You can use loops and the other functions listed below to process a list and filter elements out.
For example, you can use the `List.update_with` function to remove certain elements as described in [Updating elements](#updating-elements).

If you need to add and remove elements frequently, consider using a [set](./sets).
Sets have a dedicated `Set.remove` function that removes elements.

## Processing lists

Aside from retrieving the head and tail of a list, you can use loops and functions that iterate over every element in a list.

### Looping over lists

<Syntax syntax="cameligo">

There is no loop over lists in CameLIGO.

</Syntax>

<Syntax syntax="jsligo">

You can use a `for` loop to iterate over the elements in the list, from left to right, in the form `for (const <variable> of <list>) <block>`.
This statement means that the block of statements (or a single statement) runs once for each element in the list (`<variable>`) ranging over the elements of the list from left to right.

This example uses a `for` loop to get the sum of the integers in a list:

```jsligo group=list_looping
function sum_list (l: list<int>) {
  let sum = 0;
  for (const i of l) sum = sum + i;
  return sum;
};
```

</Syntax>

### Folding lists

Folding a list runs the same function on each element in a list and returns a single value.
The function takes two arguments: an *accumulator* and the current list element, with which it produces a new accumulator to pass to the next iteration of the function.
Folding lists allows you to compute a partial result that becomes complete when the traversal of the data structure is over.

The fold functions `List.fold_left` and `List.fold_right` take the same three parameters **in different orders**:

- The function to run on each element, which receives a tuple containing the accumulator and the current element
- The initial value for the accumulator
- The list of elements to fold

Both functions take the function to run first.
The `fold_left` function takes the initial value second and the list to fold last, while the `fold_right` function takes the list to fold second and the initial value last.

<Syntax syntax="cameligo">

These fold functions have these forms:

```
List.fold_left fold_function initial_value list
```

```
List.fold_right fold_function list initial_value
```

In this way, you can fold lists from left to right or right to left.

One way to tell them apart is to look where the folded function, and the fold itself, keep the accumulator in their signatures. Take for example a function `f`, a list `[1; 2; 3]`, and an initial accumulator `init`.
Then the `fold_left` function is equivalent to:

```
List.fold_left f init [1; 2; 3] = f (f (f (init, 1), 2), 3)
```

And the `fold_right` function is equivalent to:

```
List.fold_right f [1; 2; 3] init = f (1, f (2, f (3, init)))
```

  * The type of `List.fold_left` is `('a * 'b -> 'a) -> 'a -> 'b list
    -> 'a`.

  * The type of `List.fold_right` is `('b * 'a -> 'a) -> 'b list ->
    'a -> 'a`.

For example, here are two ways to compute the sum of integers in a list, assuming that the empty list yields `0`:

```cameligo group=folding_lists
let sum1 = List.fold_left (fun (a, i) -> a + i) 0 [1; 2; 3]
let sum2 = List.fold_right (fun (i, a) -> i + a) [1; 2; 3] 0
```

:::note

For OCaml users: In OCaml, the folded functions are curryfied, so their types are `('a -> 'b -> 'a)` `List.fold_left`, and `('b -> 'a -> 'a)` with `List.fold_right`.

:::

</Syntax>

<Syntax syntax="jsligo">

These folds have these forms:

```
List.fold_left (folded, init, list)
```

```
List.fold_right (folded, list, init)
```

:::note

The `List.fold_left` function is similar to the JavaScript `reduce` function, but note the order of the parameters.

:::

In this way, you can fold lists from left to right or right to left.

One way to tell them apart is to look where the folded function, and the fold itself, keep the accumulator in their signatures. Take for example a function `f`, a list `[1, 2, 3]`, and an initial accumulator `init`. Then

```
List.fold_left (f, init, [1;2;3]) = f (f (f (init, 1), 2), 3)
```

and

```
List.fold_right (f, [1;2;3], init) = f (1, (f (2, (f (3, init)))))
```

The type of `List.fold_left` is `(p : [a * b => a, a, b list]) => a`.

The type of `List.fold_right` is `(p : [b * a => a, b list, a]) => a`.

For example, here are two ways to compute the sum of integers in a list, assuming that the empty list yields `0`:

```jsligo group=folding_lists
const add1 = ([a, i]) => a + i;
const sum1 = List.fold_left(add1, 0, [1, 2, 3]);
const add2 = ([i, a]) => i + a;
const sum2 = List.fold_right(add2, [1, 2, 3], 0);
```

</Syntax>

### Mapping lists

Mapping a list runs the same function on each element in a list and returns a new list with the result of each function operation.
This is called a *map operation*, not to be confused with the map data type.
To map a list, use the `List.map` function, as in this example:

<Syntax syntax="cameligo">

```cameligo group=map_lists
let plus_one = List.map (fun i -> i + 1) [6; 2; 3; 3]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_lists
const plus_one = List.map(i => i + 1, [6, 2, 3, 3]);
```

</Syntax>

### Updating elements

The `List.update_with` function runs a function on each element in the list and if that function returns true, the element is replaced with a specified value.
If it returns false, the element remains.
This example takes a list of numbers and changes all of the even numbers to zero without changing the odd numbers:

<Syntax syntax="cameligo">

```cameligo group=list_updating
let nats = [0; 1; 2; 3; 4]
let evens_zeroed = List.update_with (fun x -> x mod 2 = 0n) 0 nats
// evens_zeroed = [0; 1; 0; 3; 0]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=list_updating
const nats: list<int> = [0, 1, 2, 3, 4];
const evens_zeroed = List.update_with(x => x % 2 == (0 as nat), 0, nats);
// evens_zeroed == [0, 1, 0, 3, 0]
```

</Syntax>

The function `List.update` is similar, but it replaces elements based on an option value.
If the function you pass to it returns `Some`, the value of that option becomes the new value of the element.
If it returns `None`, the value of the element stays the same.
This example takes a list of numbers and changes all of the even numbers to their square without changing the odd numbers:

<Syntax syntax="cameligo">

```cameligo group=list_updating
let f x = if x mod 2 = 0n then None else Some (x*x)
let odds_squared = List.update f nats
// odds_squared = [0; 1; 2; 9; 4]
```

</Syntax>

<Syntax syntax="jsligo">

That function takes an element and returns an optional value: if that
value is `None()`, then the element is left unchanged, otherwise, if
the value is `Some(v)`, then the element is replaced in the resulting
list by `v`.

```jsligo group=list_updating
const f = x =>
  x % 2 == (0 as nat) ? ["None" as "None"] : ["Some" as "Some", x*x];
const odds_squared = List.update(f, nats);
// odds_squared == [0, 1, 2, 9, 4]
```

</Syntax>

### Iterating over lists

The `List.iter` function is similar to the `List.map` function because it runs the same function on every element in a list.
However, the `List.iter` function returns a value of type unit, so it cannot change the list.
Therefore, this function is useful only to produce side effects, such as checking that each element of a list is within a certain range, and fail with an error otherwise.

This example iterates over a list to check that all its elements (integers) are greater than 3:

<Syntax syntax="cameligo">

```cameligo group=list_iterating
let assert_all_greater_than_3 (l : int list) : unit =
  List.iter (fun i -> Assert.assert (i > 3)) l
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=list_iterating
const assert_all_greater_than_3 =
  (l: list<int>): unit => List.iter(i => Assert.assert(i > 3), l);
```

</Syntax>
