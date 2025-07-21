---
id: comparisons
title: Comparisons
---

import Syntax from '@theme/Syntax';

In LIGO, you can compare only variables of the same type.

Moreover, you can compare only *comparable types*.
Comparable types (a concept lifted from Michelson) include:

- `int`
- `nat`
- `bytes`
- `string`
- `tez`
- `timestamp`
- `address`
- `ticket`
- `tuple`
- `variant`

Types that are not comparable include:

- `list`
- `set`
- `big-set`
- `map`
- `big-map`

Trying to compare two non-comparable types in a smart contract causes the compiler to throw an error that the types are not comparable.
If you need to compare these types, you can implement comparison logic in your own comparison functions.

<Syntax syntax="cameligo">

:::note

You can use functions such as `Test.Compare.eq` to compare some non-comparable types in tests, but these functions are not available in smart contracts.
For more information, see the predefined [module Test.Compare](../reference/test.compare-reference/?lang=cameligo).

:::

</Syntax>

<Syntax syntax="jsligo">

:::note

You can use functions such as `Test.Compare.eq` to compare some non-comparable types in tests, but these functions are not available in smart contracts.
For more information, see the predefined [namespace Test.Compare](../reference/test.compare-reference/?lang=jsligo).

:::

</Syntax>

### Comparing strings

<Syntax syntax="cameligo">

```cameligo group=strings
let a : string = "Alice"
let b : string = "Alice"
let c : bool = (a = b) (* true *)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=strings
const a = "Alice";
const b = "Alice";
const c = (a == b); // true
```

</Syntax>


### Comparing numbers

<Syntax syntax="cameligo">

```cameligo group=numbers
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

```jsligo group=numbers
const a : int = 5;
const b : int = 4;
const c : bool = (a == b);
const d : bool = (a > b);
const e : bool = (a < b);
const f : bool = (a <= b);
const g : bool = (a >= b);
const h : bool = (a != b);
```

</Syntax>

### Comparing bytes

<Syntax syntax="cameligo">

Usage:

```cameligo group=bytes
let a : bytes = 0x1001
let b : bytes = 0x1000
let c : bool = (a = b)
let d : bool = (a > b)
let e : bool = (a < b)
let f : bool = (a <= b)
let g : bool = (a >= b)
let h : bool = (a <> b)
```

</Syntax>

<Syntax syntax="jsligo">

Usage:

```jsligo group=bytes
const a : bytes = 0x1001;
const b : bytes = 0x1000;
const c : bool = (a == b);
const d : bool = (a > b);
const e : bool = (a < b);
const f : bool = (a <= b);
const g : bool = (a >= b);
const h : bool = (a != b);
```

</Syntax>

### Comparing tez

Comparing `tez` values is especially useful when dealing with an amount sent in a transaction, which you can get with the `Tezos.get_amount` function.

<Syntax syntax="cameligo">

```cameligo group=tez
let a : tez = 5mutez
let b : tez = 10mutez
let c : bool = (a = b) // false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tez
const a: tez = 5 as mutez;
const b: tez = 10 as mutez;
const c: bool = (a == b); // false
```

</Syntax>

### Comparing tuples

To compare tuples, LIGO compares the values in order from left to right.

<Syntax syntax="cameligo">

Therefore, the tuple `1, 2, 3` is equal to the tuple `1, 2, 3` but not equal to a tuple with the same values in a different order:

```cameligo group=tuples_equal
let a = 1, 2, 3
let b = 1, 2, 3
let c = (a = b) // true
let d = 3, 2, 1
let e = (a = d) // false
```

</Syntax>

<Syntax syntax="jsligo">

Therefore, the tuple `[1, 2, 3]` is equal to the tuple `[1, 2, 3]` but not equal to a tuple with the same values in a different order:

```jsligo group=tuples_equal
const a = [1, 2, 3];
const b = [1, 2, 3];
const c = (a == b); // true
const d = [3, 2, 1];
const e = (a == d); // false
```

</Syntax>

To determine whether a tuple is greater than or lesser than another tuple, LIGO compares values in the tuples from left to right until the values are different.
Then it compares the two different values and returns the result, as in this example:

<Syntax syntax="cameligo">

```cameligo group=tuples_greater
let a = 1, 3, 2
let b = 1, 2, 10
let c = (a > b) // true because 3 > 2
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tuples_greater
const a = [1, 3, 2];
const b = [1, 2, 10];
const c = (a > b); // true because 3 > 2
```

</Syntax>

### Comparing variants and options

You can compare variants and options of the same type.
To be equal, the values must have the same case and data, as in these examples:

<Syntax syntax="cameligo">

```cameligo group=variants
type user =
  Admin   of nat
| Manager of nat
| VP      of string
| Guest

let alice : user = Admin 1
let bob : user = Manager 1
let carl : user = Guest
let diana : user = VP "Accounts"
let alice_2 : user = Admin 1

let a = (alice = bob) // false
let b = (alice = carl) // false
let c = (alice = carl) // false
let d = (alice = diana) // false
let e = (alice = alice_2) // true
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=variants
type user =
  ["Admin", nat]
| ["Manager", nat]
| ["VP", string]
| ["Guest"];

const alice: user = ["Admin" as "Admin", 1 as nat];
const bob: user = ["Manager" as "Manager", 1 as nat];
const carl: user = ["Guest" as "Guest"];
const diana: user = ["VP" as "VP", "Accounts"];
const alice_2: user = ["Admin" as "Admin", 1 as nat];

const a = (alice == bob); // false
const b = (alice == carl); // false
const c = (alice == carl); // false
const d = (alice == diana); // false
const e = (alice == alice_2); // true
```

</Syntax>

You can compare variants of the same case to see if their data is greater than or less than each other:

<Syntax syntax="cameligo">

```cameligo group=variants
let edwin : user = Admin 9
let francis : user = Admin 12
let grady : user = Admin 1
let h = (edwin < francis) // true
let i = (edwin < grady) // false
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=variants
const edwin: user = ["Admin" as "Admin", 9 as nat];
const francis: user = ["Admin" as "Admin", 12 as nat];
const grady: user = ["Admin" as "Admin", 1 as nat];
const h = (edwin < francis); // true
const i = (edwin < grady); // false
```

</Syntax>

:::note

You can compare variants of different cases with the greater than and less than operators but the result is not meaningful, even if the data is of the same type.

:::
