---
id: matching
title: Matching
---

import Syntax from '@theme/Syntax';

Variant types being, in essence, the disjunctive union of cases akin
to types, values of such types need to be examined case by case: this
is what *pattern matching* does.

Here is a function that transforms a colour variant type to an integer.

<Syntax syntax="cameligo">

```cameligo group=variant_matching
type colour =
  | RGB of int * int * int
  | Gray of int
  | Default

let int_of_colour (c : colour) : int =
  match c with
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i
  | Default -> 0
```

> Note: This is the same construct as in OCaml.

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=variant_matching
type colour =
| ["RGB", [int, int, int]]
| ["Gray", int]
| ["Default"];

const int_of_colour = (c : colour) : int =>
  match(c) {
    when(RGB([r,g,b])): 16 + b + g * 6 + r * 36;
    when(Gray(i)): 232 + i;
    when(Default): 0;
  };
```

> Note: The `when`-clauses must cover all the variants of the type
> `colour`. When the constructor has no argument, which is equivalent
> to having a `[]` (unit) argument, it can be omitted, hence
> `when(Default)` instead of `when(Default())`.

The right-hand sides of each `when`-clause is an expression. Sometimes
we might need statements to be processed before a value is given to
the clause. In that case, the `do` expression comes handy. It enables
the opening of a block of statements like a function body, that is, a
block ended with a `return` statement whose argument has the value of
the block, like so:

```jsligo group=match_with_block
function match_with_block (x : option<int>) : int {
  return
    match(x) {
      when(None): 0;
      when(Some(n)): do {
        let y = n + 1;
        return y
      }
    };
};
```

</Syntax>

Another example is matching on whether an integer is a natural number
or not:

<Syntax syntax="cameligo">

```cameligo group=nat_matching
let is_it_a_nat (i : int) =
  match is_nat i with
    None   -> false
  | Some _ -> true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=nat_matching
const is_it_a_nat = (i : int) =>
  match (is_nat(i)) {
    when(None): false;
    when(Some(n)): do {ignore(n); return true; }
  }
```



</Syntax>
