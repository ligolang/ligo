---
id: looping
title: Looping
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">

The programming style promoted by CameLIGO is functional, that is,
user-defined values are constant and the preferred way to write
iterations is by means of recursive functions.

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm:

```cameligo group=looping
let rec iter (x, y : nat * nat) : nat =
  if y = 0n then x else iter (y, x mod y)

let gcd (x, y : nat * nat) : nat =
  iter (if x < y then y,x else x,y)
```

CameLIGO is not purely functional though: it also features _loops_,
which we understand as syntactic constructs where the state of a
stopping condition is mutated until it becomes true and the loop is
exited. There are two kinds of loops: `for` loops and the more general
`while` loops. Here is again Euclid's algorithm, but using mutation
(a.k.a. side effects) and a `while` loop:

```cameligo group=looping
let gcd (a, b : nat * nat) =
  let mut x, y = a, b in // We will modify x and y
  let () =
    if x < y then
      let z = x in
      begin
        x := y; y := z // Swapping x and y
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

```cameligo group=looping
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

JsLIGO currently supports iteration through _loops_, which we
understand as syntactic constructs where the state of a stopping
condition is mutated until it becomes true and the loop is
exited. There are two kinds of loops: `for` loops and the more general
`while` loops.

Here is how to compute the greatest common divisors of two natural
numbers by means of Euclid's algorithm and a `while` loop:

```jsligo group=looping
function gcd (a: nat, b: nat) {
  let [x,y] = [a,b]; // We will modify x and y
  if (x < y) {
    const z = x;
    x = y; y = z; // Swapping x and y
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

Note the use of a conditional statement to swap `x` and `y`.

Conditional logic enables forking the control flow depending on the
state, that is, the values available at a given point in the code. Put
in a less technical manner, conditionals enable decision making.

A conditional statement is made of three parts:
<ol>
  <li> a condition, that is, a boolean expression;</li>
  <li> a statement evaluated if, and only if, the condition is true;</li>
  <li> a statement evaluated if, and only if, the condition is false.</li>
</ol>

The syntax uses the keyword `if` to introduce the condition, a
statement or block of statements between `{` and `}` for the second
part, and the keyword `else` introduces the last statement or block of
statements. The last part can be omitted, as in the example above.

> Note: Currently JsLIGO does not support the keywords `break` &
> `continue` in the context of loops.

By comparison, here is how to compute the greatest common divisors of
two natural numbers by means of Euclid's algorithm using tail
recursion (no loops):

```jsligo group=looping
function iter (x: nat,y: nat): nat {
  if (y == 0n) return x else return iter (y, x % y)
};

function gcd2 (x: nat,y: nat) : nat {
  if (x < y) return iter (y, x) else return iter (x, y)
};
```

Note: The conditional statements are complete: they both feature an
`else` statement.

Finally, here is how to check if a string is a palindrome using a
`for` loop:

```jsligo group=looping
const getChar = (s: string, idx: nat) : string => String.sub(idx, 1n, s);

function isPalindrome (s: string): bool {
  let p = "";
  let length = String.length(s);
  for (let i = length - 1 ; i >= 0 ; i--)
    p += getChar(s, abs(i));
  return p == s;
};
```

> Note: The `return` statement ("early exit") is not valid in loops.

## for-of loops

JsLIGO `for-of` loops can iterate through the contents of a
collection, that is, a list, a set or a map. This is done with a loop
of the form `for (const <element var> of <collection var>) <block>`.

Here is an example where the integers in a list are summed up.

```jsligo group=looping
function sum_list (l : list<int>) {
  let acc = 0;
  for (const i of l) acc += i;
  return acc; // total
};
```

See the relevant sections on maps and sets for their loops.

</Syntax>
