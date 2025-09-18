---
title: Lists
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
const empty_list : list<int> = [];
const my_list : list<int> = [1, 2, 2]; // The head is 1, the tail is [2, 2]
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

## Adding

Lists can be augmented by adding an element before the head (or, in
terms of stack, by *pushing an element on top*). This operation is
usually called *consing* in functional languages.

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

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

The *cons operator* is infix and noted "`, ...`". It is not symmetric:
on the left lies the element to cons, and, on the right, a list on
which to cons.

```jsligo group=consing
const short_list : list<int> = [1, 2, 2];
// long_list == [5,1,2,2]
const long_list : list<int> = [5, ...short_list];
```

There is also a predefined function `List.cons`:

```jsligo group=consing
// longer_list == [6, 5, 1, 2, 2]
const longer_list = List.cons(6, long_list);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>

## Matching

Polymorphism is especially useful when writing functions over
parametric types, which include built-in types like lists, sets, and
maps.

As an example, we will see how to implement list reversing
parametrically on any type, rather than just on lists of a specific
type.

Similarly to the polymorphic identity function, we can introduce a
type variable that can be generalised. We will write a direct version
of the function using an accumulator.

<Syntax syntax="cameligo">

```cameligo group=reverse
let rev (type a) (xs : a list) : a list =
  let rec rev (type a) (xs : a list) (acc : a list) : a list =
    match xs with
    | [] -> acc
    | x :: xs -> rev xs (x::acc)
  in rev xs []
```

Note that because the type variable `a` was introduced (bound) by
means of `type`, it does not need a quote, like `'a`.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=reverse
function rev <T>(xs : list<T>) : list<T> {
  const rev = <T>([xs, acc] : [list<T>, list<T>]) : list<T> =>
    match(xs) {
      when([]): acc;
      when([y,...ys]): rev([ys, list([y,...acc])])
    };

  return rev([xs, []]);
};
```

Note how the type checker was able to infer the types of `[]` and
`[y,...ys]` in the `when` clauses (without the need of using
`[]` and `[y,...ys]`), but in `[y,...acc]` the cast
to `list` is necessary, because of the rest property that needs to be
interpreted as a cons. Similarly, the `list` in `[xs, []]` is
needed to force the interpretation of `[]` as the empty list, instead
of the empty array ("unit").

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

We use an accumulator variable `acc` to keep the elements of the list
processed, consing each element on it.

As with the identity function, we can then use `rev` directly with
different type instances:

<Syntax syntax="cameligo">

```cameligo group=reverse
let ints : int list = rev [1; 2; 3]
let nats : nat list = rev [1n; 2n; 3n]
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=reverse
const ints : list<int> = rev([1, 2, 3]);
const nats : list<nat> = rev([1n, 2n, 3n]);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>

## Updating

The function `List.update_with` enables the replacement of elements of
a given list according to a boolean function: if the call of that
function on a element is true, then the element is replaced, otherwise
it remains.

<Syntax syntax="cameligo">

```cameligo group=list_updating
let nats = [0; 1; 2; 3; 4]
// evens_zeroed = [0; 1; 0; 3; 0]
let evens_zeroed = List.update_with (fun x -> x mod 2 = 0n) 0 nats
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=list_updating
const nats : list<int> = [0, 1, 2, 3, 4];
// evens_zeroed == [0, 1, 0, 3, 0]
const evens_zeroed = List.update_with(x => x % 2 == 0n, 0, nats);
```

</Syntax>

The function `List.update` enables the selective replacement of
elements of a given list according to a function that returns an
optional value, instead of a boolean as `List.update_with` above.

<Syntax syntax="cameligo">

That function takes an element and returns an optional value: if that
value is `None`, then the element is left unchanged, otherwise, if the
value is `Some v`, then the element is replaced in the resulting list
by `v`.

```cameligo group=list_updating
let f x = if x mod 2 = 0n then None else Some (x*x)
// odds = [0; 1; 2; 9; 4]
let odds_squared = List.update f nats
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

That function takes an element and returns an optional value: if that
value is `None()`, then the element is left unchanged, otherwise, if
the value is `Some(v)`, then the element is replaced in the resulting
list by `v`.

```jsligo group=list_updating
const f = x => x % 2 == 0n ? None() : Some(x*x);
// odds == [0, 1, 2, 9, 4]
const odds_squared = List.update(f, nats);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>

## Folding

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is sometimes possible: *loops*
(see sections loops, sets and maps).

There are three kinds of functional iterations over lists: the *fold*,
the *map* (not to be confused with the *map data structure*) and the
*iteration*.

Let us consider first here the fold, which is the most general form of
functional iteration. The folded function takes two arguments: an
*accumulator* and the structure *element* at hand, with which it then
produces a new accumulator. This enables having a partial result that
becomes complete when the traversal of the data structure is over.

<Syntax syntax="cameligo">

The module `List` exports the functions `fold_left` and `fold_right`,
so folds have either the form:

```
List.fold_left folded init list
```

or

```
List.fold_right folded list init
```

which means that the folding can be done leftwards or rightwards on
the list. One way to tell them apart is to look where the folded
function, and the fold itself, keep the accumulator in their
signatures. Take for example a function `f`, a list `[1; 2; 3]`, and
an initial accumulator `init`. Then

```
List.fold_left f init [1; 2; 3] = f (f (f (init, 1), 2), 3)
```

and

```
List.fold_right f [1; 2; 3] init = f (1, f (2, f (3, init)))
```

  * The type of `List.fold_left` is `('a * 'b -> 'a) -> 'a -> 'b list
    -> 'a`.

  * The type of `List.fold_right` is `('b * 'a -> 'a) -> 'b list ->
    'a -> 'a`.

For example, let us compute the sum of integers in a list, assuming
that the empty list yields `0`:

```cameligo group=folding_lists
let sum1 = List.fold_left (fun (a,i) -> a + i) 0 [1; 2; 3]
let sum2 = List.fold_right (fun (i,a) -> i + a) [1; 2; 3] 0
```

> For OCaml users: In OCaml, the folded functions are curryfied, so
> their types are `('a -> 'b -> 'a)` `List.fold_left`, and `('b -> 'a
> -> 'a)` with `List.fold_right`.

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

The module `List` exports the functions `fold_left` and `fold_right`,
so folds have either the form:

```
List.fold_left (folded, init, list)
```

or

```
List.fold_right (folded, list, init)
```

which mean that the folding can be done leftwards or rightwards on the
list. One way to tell them apart is to look where the folded function,
and the fold itself, keep the accumulator in their signatures. Take
for example a function `f`, a list `[1, 2, 3]`, and an initial
accumulator `init`. Then

```
List.fold_left (f, init, [1;2;3]) = f (f (f (init, 1), 2), 3)
```

and

```
List.fold_right (f, [1;2;3], init) = f (1, (f (2, (f (3, init)))))
```

The type of `List.fold_left` is `(p : [a * b => a, a, b list]) => a`.

The type of `List.fold_right` is `(p : [b * a => a, b list, a]) => a`.

For example, let us compute the sum of integers in a list, assuming
that the empty list yields `0`:

```jsligo group=folding_lists
const add1 = ([a, i]) => a + i;
const sum1 = List.fold_left(add1, 0, [1, 2, 3]);
const add2 = ([i, a]) => i + a;
const sum2 = List.fold_right(add2, [1, 2, 3], 0);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>

## Mapping

We may want to change all the elements of a given list by applying to
them a function. This is called a *map operation*, not to be confused
with the map data structure. The predefined functional iterator
implementing the mapped operation over lists is called `List.map` and
is used as follows.

<Syntax syntax="cameligo">

```cameligo group=map_lists
let plus_one = List.map (fun i -> i + 1) [6; 2; 3; 3]
```

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=map_lists
const plus_one = List.map(i => i + 1, [6, 2, 3, 3]);
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>

## Looping

<Syntax syntax="cameligo">
There is no loop over lists in CameLIGO.

See predefined
[module List](../reference/list-reference/?lang=cameligo).

</Syntax>

<Syntax syntax="jsligo">

One can iterate through all the elements of a list, from left to
right, thanks to a loop of the form `for (const <variable> of <list>) <block>`. It means that the `<block>` of statements (or a single
statement) will be computed once for each `<variable>` ranging over
the elements of the list `<list>` from left to right.

Here is an example where the integers in a list are summed up, and the
sum is zero if the list is empty:

```jsligo group=list_looping
function sum_list (l: list<int>) {
  let sum = 0;
  for (const i of l) sum = sum + i;
  return sum;
};
```

See predefined
[namespace List](../reference/list-reference/?lang=jsligo).

</Syntax>
