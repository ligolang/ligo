---
id: loops
title: Iteration
---

import Syntax from '@theme/Syntax';

## General Iteration

<Syntax syntax="cameligo">

CameLIGO is a functional language where user-defined values are
constant, therefore it makes no sense in CameLIGO to feature loops,
which we understand as syntactic constructs where the state of a
stopping condition is mutated, as with "while" loops in PascaLIGO.

Instead, CameLIGO loops are written by means of a tail recursive function

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm:

```cameligo group=a
let rec iter (x, y : nat * nat) : nat =
  if y = 0n then x else iter (y, x mod y)

let gcd (x, y : nat * nat) : nat =
  let x, y = if x < y then y,x else x,y
  in iter (x, y)
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.mligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd
# Outputs: +12
```

</Syntax>

<Syntax syntax="jsligo">

JsLIGO currently supports iteration through while while loops and
through the use of tail recursive functions.

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm with using a while loop:


```jsligo group=a
let gcd = (x: nat, y: nat) => {
  let [x,y] = [x,y]; // we will modify x and y
  if (x < y) {
    const z = x;
    x = y; y = z;
  };
  let r: nat = 0 as nat;
  while (y != (0 as nat)) {
    r = x % y;
    x = y;
    y = r;
  }
  return x
}
```

And here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm using tail recursion:

```jsligo group=a
let iter = (x: nat,y: nat): nat => {
  if (y == (0 as nat)) {
    return x;
  } else {
    return iter (y, x % y);
  };
};

let gcd2 = (x: nat,y: nat) : nat => {
  if (x < y) {
    return iter (y, x);
  } else {
    return iter (x, y);
  }
};
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.jsligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd
# Outputs: +12
```

and can call the function `gcd2` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/gcd.jsligo '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)' --entry-point gcd2
# Outputs: +12
```

</Syntax>

<Syntax syntax="jsligo">

## for-of Loops

JsLIGO "for-of" loops can iterate through the contents of a
collection, that is, a list, a set or a map. This is done with a loop
of the form `for (const <element var> of <collection var>) <block>`.

Here is an example where the integers in a list are summed up.

```jsligo group=d
let sum_list = (l : list<int>) => {
  let total = 0;
  for (const i of l) {
    total = total + i
  }
  return total
}
```

You can call the function `sum_list` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.jsligo --entry-point sum_list
'list [1;2;3]'
# Outputs: 6
```

Here is an example where the integers in a set are summed up.

```jsligo group=d
let sum_set = (s : set<int>) => {
  let total : int = 0;
  for (const i of s) {
    total = total + i
  };
  return total
}
```

You can call the function `sum_set` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.jsligo --entry-point sum_set
'set [1;2;3]'
# Outputs: 6
```

Loops over maps are actually loops over the bindings of the map.
Given a map from strings to integers, here is how to sum
all the integers and concatenate all the strings.


```jsligo
let sum_map = (m: map<string, int>) => {
  let string_total = "";
  let int_total = 0;
  for (const item of m) {
    let [key, value] = item;
    string_total = string_total + key;
    int_total = int_total + value
  };
  return [string_total, int_total]
}
```

You can call the function `sum_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/loops/collection.jsligo --entry-point sum_map
'map ["1"->1; "2"->2; "3"->3]'
# Outputs: ( "123", 6 )
```

</Syntax>
