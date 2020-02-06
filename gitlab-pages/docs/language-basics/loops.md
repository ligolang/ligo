---
id: loops
title: Loops
---

## General Iteration

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

General iteration in PascaLIGO takes the shape of "while" loops, which
should be familiar to programmers of imperative languages. Those loops
are of the form `while <condition> <block>`. Their associated block is
repeatedly evaluated until the condition becomes true, or never
evaluated if the condition is false at the start. The loop never
terminates if the condition never becomes true. Because we are writing
smart contracts on Tezos, when the condition of a "while" loops fails
to become true, the execution will run out of gas and stop with a
failure anyway.

Here is how to compute the greatest common divisors of two natural
number by means of Euclid's algorithm:

```pascaligo group=a
function gcd (var x : nat; var y : nat) : nat is block {
  if x < y then
    block {
      const z : nat = x;
      x := y; y := z
    }
  else skip;
  var r : nat := 0n;
  while y =/= 0n block {
    r := x mod y;
    x := y;
    y := r
  }
} with x
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/loops/gcd.ligo gcd '(2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)'
# Outputs: +12
```

<!--CameLIGO-->

CameLIGO is a functional language where user-defined values are
constant, therefore it makes no sense in CameLIGO to feature loops,
which we understand as syntactic constructs where the state of a
stopping condition is mutated, as with "while" loops in PascaLIGO.

Instead, CameLIGO features a *fold operation* as a predefined function
named `Loop.fold_while`. It takes an initial value of a certain type,
called an *accumulator*, and repeatedly calls a given function, called
*iterated function*, that takes that accumulator and returns the next
value of the accumulator, until a condition is met and the fold stops
with the final value of the accumulator. The iterated function needs
to have a special type: if the type of the accumulator is `t`, then it
must have the type `bool * t` (not simply `t`). It is the boolean
value that denotes whether the stopping condition has been reached.

```cameligo group=a
let iter (x,y : nat * nat) : bool * (nat * nat) =
  if y = 0n then false, (x,y) else true, (y, x mod y)

let gcd (x,y : nat * nat) : nat =
  let x,y = if x < y then y,x else x,y in
  let x,y = Loop.fold_while iter (x,y)
  in x
```

To ease the writing and reading of the iterated functions (here,
`iter`), two predefined functions are provided: `continue` and `stop`:

```cameligo group=a
let iter (x,y : nat * nat) : bool * (nat * nat) =
  if y = 0n then stop (x,y) else continue (y, x mod y)

let gcd (x,y : nat * nat) : nat =
  let x,y = if x < y then y,x else x,y in
  let x,y = Loop.fold_while iter (x,y)
  in x
```

You can call the function `gcd` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/loops/gcd.mligo gcd (2n*2n*3n*11n, 2n*2n*2n*3n*3n*5n*7n)'
# Outputs: +12
```

<!--ReasonLIGO-->

ReasonLIGO is a functional language where user-defined values are
constant, therefore it makes no sense in ReasonLIGO to feature loops,
which we understand as syntactic constructs where the state of a
stopping condition is mutated, as with "while" loops in PascaLIGO.

Instead, ReasonLIGO features a *fold operation* as a predefined
function named `Loop.fold_while`. It takes an initial value of a
certain type, called an *accumulator*, and repeatedly calls a given
function, called *iterated function*, that takes that accumulator and
returns the next value of the accumulator, until a condition is met
and the fold stops with the final value of the accumulator. The
iterated function needs to have a special type: if the type of the
accumulator is `t`, then it must have the type `bool * t` (not simply
`t`). It is the boolean value that denotes whether the stopping
condition has been reached.

```reasonligo group=a
let iter = ((x,y) : (nat, nat)) : (bool, (nat, nat)) =>
  if (y == 0n) { (false, (x,y)); } else { (true, (y, x mod y)); };

let gcd = ((x,y) : (nat, nat)) : nat => {
  let (x,y) = if (x < y) { (y,x); } else { (x,y); };
  let (x,y) = Loop.fold_while (iter, (x,y));
  x
};
```

To ease the writing and reading of the iterated functions (here,
`iter`), two predefined functions are provided: `continue` and `stop`:

```reasonligo group=b
let iter = ((x,y) : (nat, nat)) : (bool, (nat, nat)) =>
  if (y == 0n) { stop ((x,y)); } else { continue ((y, x mod y)); };

let gcd = ((x,y) : (nat, nat)) : nat => {
  let (x,y) = if (x < y) { (y,x); } else { (x,y); };
  let (x,y) = Loop.fold_while (iter, (x,y));
  x
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

## For Loops

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

To iterate over a range of integers you use a loop of the form `for
<variable assignment> to <upper integer bound> <block>`, which is
familiar for programmers of imperative languages. Note that, for the
sake of generality, the bounds are of type `int`, not `nat`.

```pascaligo group=c
function sum (var n : nat) : int is block {
  var acc : int := 0;
  for i := 1 to int (n) block {
    acc := acc + i
  }
} with acc
```

You can call the function `sum` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/loops/sum.ligo sum 7n
# Outputs: 28
```
<!--END_DOCUSAURUS_CODE_TABS-->

<!--DOCUSAURUS_CODE_TABS-->

<!--Pascaligo-->

PascaLIGO "for" loops can also iterate through the contents of a
collection, that is, a list, a set or a map. This is done with a loop
of the form `for <element var> in <collection type> <collection var>
<block>`, where `<collection type` is any of the following keywords:
`list`, `set` or `map`.

Here is an example where the integers in a list are summed up.

```pascaligo group=d
function sum_list (var l : list (int)) : int is block {
  var total : int := 0;
  for i in list l block {
    total := total + i
  }
} with total
```

You can call the function `sum_list` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/loops/collection.ligo sum_list
'list [1;2;3]'
# Outputs: 6
```

Here is an example where the integers in a set are summed up.

```pascaligo=e
function sum_set (var s : set (int)) : int is block {
  var total : int := 0;
  for i in set s block {
    total := total + i
  }
} with total
```

You can call the function `sum_set` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/loops/collection.ligo sum_set
'set [1;2;3]'
# Outputs: 6
```

Loops over maps are actually loops over the bindings of the map, that
is, a pair key-value noted `key -> value` (or any other
variables). Give a map from strings to integers, here is how to sum
all the integers and concatenate all the strings.

You can call the function `sum_map` defined above using the LIGO compiler
like so:
```shell
ligo run-function
gitlab-pages/docs/language-basics/src/loops/collection.ligo sum_map
'map ["1"->1; "2"->2; "3"->3]'
# Outputs: ( "123", 6 )
```
<!--END_DOCUSAURUS_CODE_TABS-->
