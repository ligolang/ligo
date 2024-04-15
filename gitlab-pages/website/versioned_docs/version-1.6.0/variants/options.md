---
id: options
title: Options
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

The `option` type is a parametric, predefined variant type that is
used to express whether there is a value of some type or none. This is
especially useful when calling a *partial function*, that is, a
function that is not defined for some inputs. In that case, the value
of the `option` type would be `None`, otherwise `Some (v)`, where `v`
is some meaningful value *of any type*. A typical example from
arithmetics is the division:

```cameligo group=options
let div (a, b : nat * nat) : nat option =
  if b = 0n then None else Some (a/b)
```

Note: See the predefined
[module Option](../reference/option-reference/?lang=cameligo)

</Syntax>

<Syntax syntax="jsligo">

The `option` type is a parametric, predefined variant type that is
used to express whether there is a value of some type or none. This is
especially useful when calling a *partial function*, that is, a
function that is not defined for some inputs. In that case, the value
of the `option` type would be `None()`, otherwise `Some(v)`, where `v`
is some meaningful value *of any type*. A typical example from
arithmetics is the division:

```jsligo group=options
function div (a: nat, b: nat): option<nat> {
  if (b == 0n) return None() else return Some(a/b)
};
```

Note: See the predefined
[namespace Option](../reference/option-reference/?lang=jsligo)

</Syntax>

## Euclidean Division

<Syntax syntax="cameligo">

For cases when you need both the quotient and the remainder, LIGO
provides the `ediv` operation. `ediv x y` returns `Some (quotient,
remainder)`, unless `y` is zero, in which case it returns `None`. The
function `ediv` is overloaded to accept all the combinations (4) of
natural and integer numbers:

```cameligo group=options_euclidean
// All below equal Some (7,2)
let ediv1 : (int * nat) option = ediv 37  5
let ediv2 : (int * nat) option = ediv 37n 5
let ediv3 : (nat * nat) option = ediv 37n 5n
let ediv4 : (int * nat) option = ediv 37  5n
```

</Syntax>

<Syntax syntax="jsligo">

For cases when you need both the quotient and the remainder, LIGO
provides the `ediv` operation. `ediv(x,y)` returns `Some (quotient,
remainder)`, unless `y` is zero, in which case it returns `None`. The
function `ediv` is overloaded to accept all the combinations (4) of
natural and integer numbers:

```jsligo group=options_euclidean
// All below equal Some (7,2)
const ediv1: option<[int, nat]> = ediv(37,  5);
const ediv2: option<[int, nat]> = ediv(37n, 5);
const ediv3: option<[nat, nat]> = ediv(37n, 5n);
const ediv4: option<[int, nat]> = ediv(37,  5n);
```

</Syntax>

## Checking positivity

You can check if a value is a natural number (`nat`) by using a
predefined cast function which accepts an integer (`int`) and returns
an optional natural number (`nat`): if the result is `None`, then the
given integer was positive, otherwise the corresponding natural number
`n` is given with `Some(n)`.

<Syntax syntax="cameligo">

```cameligo group=options_positive
let one_is_nat : nat option = is_nat (1)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=options_positive
const one_is_nat : option<nat> = is_nat(1);
```

</Syntax>
