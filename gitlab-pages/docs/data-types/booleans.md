---
id: booleans
title: Booleans
---

import Syntax from '@theme/Syntax';

The predefined Boolean type `bool` has exactly two values: `true` and `false`.

<Syntax syntax="cameligo">

```cameligo group=booleans
let a : bool = true
let b : bool = false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=booleans
const a: bool = true;
const b: bool = false;
```

</Syntax>

## Or

<Syntax syntax="cameligo">

The logical disjunction ("or") is implemented by the binary operator `||`:

```cameligo group=or
let or_1 : bool = false || true  // true
let or_2 : bool = false || false // false
let or_3 : bool = true  || true  // true
let or_4 : bool = true  || false // true
```

You can also use the keyword `or` instead of the symbol `||` (as in OCaml):

```cameligo group=or
let or_1 : bool = false or true  // true
let or_2 : bool = false or false // false
let or_3 : bool = true  or true  // true
let or_4 : bool = true  or false // true
```

</Syntax>

<Syntax syntax="jsligo">

The logical disjunction ("or") is implemented by the binary operator `||`.

```jsligo group=or
const or_1: bool = false || true;  // true
const or_2: bool = false || false; // false
const or_3: bool = true  || true;  // true
const or_4: bool = true  || false; // true
```

</Syntax>

## And

The logical conjunction ("and") is implemented by the binary operator `&&`.

<Syntax syntax="cameligo">

```cameligo group=conjunction
let and_1 : bool = false && true  // false
let and_2 : bool = false && false // false
let and_3 : bool = true  && true  // true
let and_4 : bool = true  && false // false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=conjunction
const and_1: bool = false && true;  // false
const and_2: bool = false && false; // false
const and_3: bool = true  && true;  // true
const and_4: bool = true  && false; // false
```

</Syntax>

## Not

<Syntax syntax="cameligo">

The logical negation ("not") is implemented by the unary operator `not`.

```cameligo group=not
let not_1 : bool = not true  // false
let not_2 : bool = not false // true
```

</Syntax>

<Syntax syntax="jsligo">

The logical negation ("not") is implemented by the unary operator `!`.

```jsligo group=not
const not_1: bool = !true  // false
const not_2: bool = !false // true
```

</Syntax>

## Comparing

Boolean values are the result of comparisons of other values.
As described in [Comparisons](../syntax/comparisons), values must be the same type to be compared, and not all types are comparable.
You can compare comparable types such as `int`, `nat`, `string`, and `bool` to each other.
The comparison operators are overloaded so they are defined on all comparable types, as in these examples:

<Syntax syntax="cameligo">

```cameligo group=comparing
let a : bool = 1 = 1   // equality (true)
let b : bool = 1 <> 0  // inequality (true)
let c : bool = 1 > 0   // greater than (true)
let d : bool = 0 < 1   // lower than (true)
let e : bool = 0 >= 0  // greater than or equal (true)
let f : bool = 0 <= 0  // lower than or equal (true)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=comparing
const a: bool = 1 == 1;  // equality (true)
const b: bool = 1 != 0;  // inequality (true)
const c: bool = 1 > 0;   // greater than (true)
const d: bool = 0 < 1;   // lower than (true)
const e: bool = 0 >= 0;  // greater than or equal (true)
const f: bool = 0 <= 0;  // lower than or equal (true)
```

</Syntax>

You can also use a single value to create a Boolean value.
For example, empty strings and the number 0 are false, while strings with any content in them and nonzero numbers are true:

<Syntax syntax="cameligo">

```cameligo group=unary_boolean
let natToBoolTrue : bool = 1n      (* True *)
let natToBoolFalse : bool = 0n     (* False *)
let stringToBoolTrue : bool = "A"  (* True *)
let stringToBoolFalse : bool = ""  (* False *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=unary_boolean
const natToBoolTrue: bool = (1 as nat);   // True
const natToBoolFalse: bool = (0 as nat);  // False
const stringToBoolTrue: bool = "A";       // True
const stringToBoolFalse: bool = "";       // False
```

</Syntax>

For more information about comparing values, see [Comparisons](../syntax/comparisons).

You can use Boolean values and comparisons in logical `if` and `else` statements like many other languages; see [Conditionals](../imperative/conditionals).
