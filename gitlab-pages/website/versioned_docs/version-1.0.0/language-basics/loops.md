---
id: loops
title: Iteration
---

import Syntax from '@theme/Syntax';

## General Iteration

<Syntax syntax="cameligo">

CameLIGO is a functional language where user-defined values are
constant, therefore the preferred way to write iterations is by means
of recursive functions. Here is how to compute the greatest common
divisors of two natural numbers by means of Euclid's algorithm:

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

CameLIGO also features loops, which we understand as syntactic
constructs where the state of a stopping condition is mutated. There
are two kinds of loops: for-loops and while-loops. Here is again
Euclid's algorithm, but using mutation and a while-loop:

```cameligo group=a
let gcd (a, b : nat * nat) =
  let mut x, y = a, b in // we will modify x and y
  let () =
    if x < y then
      let z = x in
      begin
        x := y; y := z
      end in
  let mut r : nat = 0n in
  let () =
    while y <> 0n do
      r := x mod y;
      x := y;
      y := r
    done
  in x
```

Here is how to check if a string is a palindrome or not using a `for` loop:

```cameligo group=a
let get_char s idx = String.sub idx 1n s

let is_palindrome s =
  let mut p = "" in
  let length = String.length s in
  let () =
    for i = length - 1 downto 0 do
      p := p ^ get_char s (abs i)
    done
  in p = s
```


</Syntax>

<Syntax syntax="jsligo">

JsLIGO currently supports iteration through while-loops, for-loops,
and through the use of tail recursive functions.

Here is how to check if a string is a palindrome or not using a `for` loop:

```jsligo group=a
const getChar = (s: string, idx: nat): string => String.sub(idx, 1n, s);

function isPalindrome (s: string): bool {
  let p = "";
  let length = String.length(s);
  for (let i = length - 1 ; i >= 0 ; i--)
    p += getChar(s, abs(i));
  return p == s;
};
```

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm with using a while loop:

```jsligo group=a
function gcd (a: nat, b: nat) {
  let [x,y] = [a,b]; // we will modify x and y
  if (x < y) {
    const z = x;
    x = y; y = z;
  }
  let r: nat = 0n
  while (y != 0n) {
    r = x % y;
    x = y;
    y = r;
  }
  return x;
};
```

And here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm using tail recursion:

```jsligo group=a
function iter (x: nat,y: nat): nat {
  if (y == 0n) return x else return iter (y, x % y)
};

function gcd2 (x: nat,y: nat) : nat {
  if (x < y) return iter (y, x) else return iter (x, y)
};
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/loops/gcd.jsligo \
  'gcd(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)'
# Outputs: +12
```

and can call the function `gcd2` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/loops/gcd.jsligo \
  'gcd2(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)'
# Outputs: +12
```

> Note: Currently JsLIGO does not support the key words `break` & `continue` in the context
> of loops.

</Syntax>

<Syntax syntax="jsligo">

## for-of Loops

JsLIGO "for-of" loops can iterate through the contents of a
collection, that is, a list, a set or a map. This is done with a loop
of the form `for (const <element var> of <collection var>) <block>`.

Here is an example where the integers in a list are summed up.

```jsligo group=d
function sum_list (l : list<int>) {
  let total = 0;
  for (const i of l) total = total + i;
  return total;
};
```

You can call the function `sum_list` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/loops/collection.jsligo \
  'sum_list(list([1,2,3]))'
# Outputs: 6
```

Here is an example where the integers in a set are summed up.

```jsligo group=d
function sum_set (s : set<int>) {
  let total : int = 0;
  for (const i of s) total = total + i;
  return total;
};
```

You can call the function `sum_set` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/loops/collection.jsligo \
  'sum_set(Set.literal(list([1;2;2;3])))'
# Outputs: 6
```

Loops over maps are actually loops over the bindings of the map.
Given a map from strings to integers, here is how to sum
all the integers and concatenate all the strings.


```jsligo
function sum_map (m: map<string, int>) {
  let string_total = ""
  let int_total = 0
  for (const item of m) {
    let [key, value] = item;
    string_total = string_total + key;
    int_total = int_total + value
  }
  return [string_total, int_total]
}
```

You can call the function `sum_map` defined above using the LIGO compiler
like so:
```shell
ligo run evaluate-expr \
  gitlab-pages/docs/language-basics/src/loops/collection.jsligo \
 'sum_map(Map.literal(list([ ["1", 1], ["2", 2], ["3", 3] ])))'
# Outputs: ( "123", 6 )
```

</Syntax>

<!-- updated use of entry -->