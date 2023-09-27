---
id: boolean-if-else
title: Booleans and Conditionals
---

import Syntax from '@theme/Syntax';

## Booleans

The type of a boolean value is `bool`. Here is how to define a boolean
value:

<Syntax syntax="cameligo">

```cameligo group=a
let a : bool = true
let b : bool = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=a
const a = true;
const b = false;
```

</Syntax>

Common operations:

<Syntax syntax="cameligo">
<div className="boolean-example-table">
  <div className="operation">
    &&
  </div>
  <div className="description">
    Logical and
  </div>
  <div className="example">

```cameligo
let logical_and: bool = true && true
```

  </div>
  <div className="operation">
    ||
  </div>
  <div className="description">
    Logical or
  </div>
  <div className="example">

```cameligo
let logical_or: bool = false || true
```

  </div>
  <div className="operation">
    not
  </div>
  <div className="description">
    Logical not
  </div>
  <div className="example">

```cameligo
let logical_not: bool = not false
```

  </div>
  <div className="operation">
    =
  </div>
  <div className="description">
    Equals
  </div>
  <div className="example">

```cameligo
let eq: bool = 2 = 3
```

  </div>
  <div className="operation">
    &lt;&gt;
  </div>
  <div className="description">
    Not equals
  </div>
  <div className="example">

```cameligo
let not_eq: bool = 2 <> 3
```

  </div>
  <div className="operation">
    &gt;
  </div>
  <div className="description">
    Greater than
  </div>
  <div className="example">

```cameligo
let gt: bool = 4 > 3
```

  </div>
  <div className="operation">
    &lt;
  </div>
  <div className="description">
    Less than
  </div>
  <div className="example">

```cameligo
let lt: bool = 4 < 3
```

  </div>
  <div className="operation">
    &gt;=
  </div>
  <div className="description">
    Greater than or equal to
  </div>
  <div className="example">

```cameligo
let gte: bool = 4 >= 3
```

  </div>
  <div className="operation">
    &lt;=
  </div>
  <div className="description">
    Less than or equal to
  </div>
  <div className="example">

```cameligo
let lte: bool = 4 <= 3
```

  </div>
</div>
</Syntax>


<Syntax syntax="jsligo">
<div className="boolean-example-table">
  <div className="operation">
    &&
  </div>
  <div className="description">
    Logical and
  </div>
  <div className="example">

```jsligo
const logical_and = true && true;
```

  </div>
  <div className="operation">
    ||
  </div>
  <div className="description">
    Logical or
  </div>
  <div className="example">

```jsligo
const logical_or = false || true;
```

  </div>
  <div className="operation">
    !
  </div>
  <div className="description">
    Logical not
  </div>
  <div className="example">

```jsligo
const logical_not = !false;
```

  </div>
  <div className="operation">
    ==
  </div>
  <div className="description">
    Equals
  </div>
  <div className="example">

```jsligo
const eq = 2 == 3;
```

  </div>
  <div className="operation">
    !=
  </div>
  <div className="description">
    Not equals
  </div>
  <div className="example">

```jsligo
const not_eq = 2 != 3;
```

  </div>
  <div className="operation">
    &gt;
  </div>
  <div className="description">
    Greater than
  </div>
  <div className="example">

```jsligo
const gt = 4 > 3;
```

  </div>
  <div className="operation">
    &lt;
  </div>
  <div className="description">
    Less than
  </div>
  <div className="example">

```jsligo
const lt = 4 < 3;
```

  </div>
  <div className="operation">
    &gt;=
  </div>
  <div className="description">
    Greater than or equal to
  </div>
  <div className="example">

```jsligo
const gte = 4 >= 3;
```

  </div>
  <div className="operation">
    &lt;=
  </div>
  <div className="description">
    Less than or equal to
  </div>
  <div className="example">

```jsligo
const lte = 4 <= 3;
```

  </div>
</div>
</Syntax>


## Comparing Values

In LIGO, only values of the same type can be compared. Moreover, not
all values of the same type can be compared, only those with
*comparable types*, which is a concept lifted from
Michelson. Comparable types include, for instance, `int`, `nat`, `bytes`
`string`, `tez`, `timestamp`, `address`, etc. As an example of
non-comparable types: maps, sets or lists are not comparable: if you
wish to compare them, you will have to write your own comparison
function.

> Note: when running in test mode (this is, in the testing framework),
> for developer convinence, more types are made comparable. Maps, sets
> and lists will be made comparable in case its elements are
> comparable.

### Comparing Strings

<Syntax syntax="cameligo">

```cameligo group=b
let a : string = "Alice"
let b : string = "Alice"
let c : bool = (a = b) (* true *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=b
const a = "Alice";
const b = "Alice";
const c = (a == b); // true
```

</Syntax>


### Comparing numbers

<Syntax syntax="cameligo">

```cameligo group=c
let a : int  = 5
let b : int  = 4
let c : bool = (a = b)
let d : bool = (a > b)
let e : bool = (a < b)
let f : bool = (a <= b)
let g : bool = (a >= b)
let h : bool = (a <> b)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=c
const a  = 5;
const b  = 4;
const c = (a == b);
const d = (a > b);
const e = (a < b);
const f = (a <= b);
const g = (a >= b);
const h = (a != b);
```

</Syntax>

### Comparing bytes

<Syntax syntax="cameligo">

To check if the following operators have the expected result use
`ligo compile expression cameligo "a OP b"`

Usage:

```cameligo group=d
let a : bytes  = 0x1001
let b : bytes  = 0x1000
let c : bool = (a = b)
let d : bool = (a > b)
let e : bool = (a < b)
let f : bool = (a <= b)
let g : bool = (a >= b)
let h : bool = (a <> b)
```

</Syntax>
<Syntax syntax="jsligo">


To check if the following operators have the expected result use
`ligo compile expression jsligo "a OP b"`

Usage:

```jsligo group=d
const a = 0x1001;
const b = 0x1000;
const c = (a == b);
const d = (a > b);
const e = (a < b);
const f = (a <= b);
const g = (a >= b);
const h = (a != b);
```

</Syntax>

### Comparing tez

> 💡 Comparing `tez` values is especially useful when dealing with an
> amount sent in a transaction.

<Syntax syntax="cameligo">

```cameligo group=e
let a : tez  = 5mutez
let b : tez  = 10mutez
let c : bool = (a = b) // false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=e
const a: tez  = 5mutez;
const b: tez  = 10mutez;
const c = (a == b); // false
```

</Syntax>



## Conditionals

Conditional logic enables forking the control flow depending on the
state.

<Syntax syntax="cameligo">

```cameligo group=e
type magnitude = Small | Large (* See variant types. *)

let compare (n : nat) : magnitude =
  if n < 10n then Small else Large
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call
gitlab-pages/docs/language-basics/src/boolean-if-else/cond.mligo '21n' --entry-point compare
# Outputs: Large
```

> Notice that, as in OCaml, in CameLIGO, if a conditional has a branch
> `else ()`, that branch can be omitted. The resulting so-called
> *dangling else* problem is parsed by associating any `else` to the
> closest previous `then`.

</Syntax>
<Syntax syntax="jsligo">

```jsligo group=e
type magnitude = ["Small"] | ["Large"]; // See variant types.

const compare = (n) => {
  if (n < 10n) return Small() else return Large()
};
```

You can run the `compare` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call  gitlab-pages/docs/language-basics/src/boolean-if-else/cond.jsligo compare '21n'
# Outputs: Large
```

</Syntax>



<Syntax syntax="jsligo">

## Switch Statement

JsLIGO also supports branching of control flow via the switch statement.

```jsligo group=switch
let quarter = n => {
  let output = "";
  switch (n) {
    case 1:
    case 2:
    case 3:
      output = "Q1";
      break;
    case 4:
    case 5:
    case 6:
      output = "Q2";
      break;
    case 7:
    case 8:
    case 9:
      output = "Q3";
      break;
    case 10:
    case 11:
    case 12:
      output = "Q4";
      break;
    default:
      output = "Invalid month."
  };
  return output;
}
```

The switch statement takes an expression and tries to find a `case` which matches the switch expression,
If a matching `case` is found, the statements of the matching case are executed untill a `break;` statement.
If no `break` is found the control falls through to the next `case` or `default`. If no matching case is found
the statements of the `default` case are executed.

> A few gotcha's about the switch statement
> 1. A switch should have at-least one `case` or `default`.
> 2. If a `default` case is provided, It should be the last case.
> 3. Conditional `break`'s are not supported i.e. `break` inside a `if-then-else`.
> 4. In case of nested `switch` statements, the inner `switch` should not contain a `return`.

You can run the `quarter` function defined above using the LIGO compiler
like this:
```shell
ligo run evaluate-call  gitlab-pages/docs/language-basics/src/boolean-if-else/switch.jsligo quarter '5'
# Outputs: "Q2"
```

### Ternary conditional expression
JsLIGO also supports JavaScript's ternary expression:

```jsligo
const ternary = a => a == 1 ? true : false;
```

which can also be nested:

```jsligo
const ternary_nested = a =>
  a == 1 ? "one"   :
  a == 2 ? "two"   :
  a == 3 ? "three" :
           "other"
```

</Syntax>

<!-- updated use of entry -->