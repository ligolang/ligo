---
id: cheat-sheet
title: Cheat Sheet
---

import Syntax from '@theme/Syntax';

<div className="cheatsheet">

<Syntax syntax="cameligo">

<div className="codeTable">
<div className="primitive">Strings</div>
<div className="example">

```cameligo
let name : string = "Tezos"
```

</div>
<div className="primitive">
Characters
</div>
<div className="example">

```cameligo
let t : string = "t"
```

</div>
<div className="primitive">
Integers
</div>
<div className="example">

```cameligo
let i : int = 42
```

</div>
<div className="primitive">
Natural numbers
</div>
<div className="example">

```cameligo
let n : nat = 7n
```

</div>
<div className="primitive">
Unit
</div>
<div className="example">

```cameligo
let u : unit = unit
```

</div>
<div className="primitive">
Boolean
</div>
<div className="example">

```cameligo
let has_drivers_license : bool = false
let adult : bool = true
```

</div>
<div className="primitive">
Boolean Logic
</div>
<div className="example">

```cameligo
let booleanLogic : bool =
    (not true) =
    false =
    (false && true) =
    (false || false)
```

</div>
<div className="primitive">
Mutez (micro tez)
</div>
<div className="example">

```cameligo
let tez : tez = 42tez
let tez : tez = 7mutez
```

</div>
<div className="primitive">
Address
</div>
<div className="example">

```cameligo
let tz1address : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let kt1address : address =
  ("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" : address)
```

</div>
<div className="primitive">
Addition
</div>
<div className="example">

```cameligo
let add_int : int = 3 + 4
let add_nat : nat = 3n + 4n
```

</div>
<div className="primitive">
Multiplication & Division
</div>
<div className="example">

```cameligo
let mul_int : int = 3 * 4
let mul_nat : nat = 3n * 4n

let div_int : int = 10 / 5
let div_nat : nat = 10n / 5n
```

</div>
<div className="primitive">
Modulo
</div>
<div className="example">

```cameligo
let mod_nat : nat = 10 mod 3
```

</div>
<div className="primitive">
Tuples
</div>
<div className="example">

```cameligo
type name = string * string

let winner : name = "John", "Doe"

let firstName : string = winner.0
let lastName : string = winner.1
```

</div>
<div className="primitive">
Types
</div>
<div className="example">

```cameligo
type age = int
type name = string
```

</div>
<div className="primitive">
Includes
</div>
<div className="example">

```#include "library.mligo"```

</div>
<div className="primitive">
Functions
</div>
<div className="example">

```cameligo
let add (a : int) (b : int) : int =
  a + b
```

</div>

<div className="primitive">
If Statement
</div>
<div className="example">

```cameligo
let can_drive (age : nat) : string =
  if age >= 16n then "yes" else "no"
```

</div>
<div className="primitive">
Options
</div>
<div className="example">

```cameligo
type middle_name = string option
let middle_name : middle_name = Some "Foo"
let middle_name : middle_name = None
```

</div>
<div className="primitive">
Variable Binding
</div>
<div className="example">

```cameligo
let age : int = 5
```

</div>
<div className="primitive">
Type Annotations
</div>
<div className="example">

```cameligo
let someAddress : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
```

</div>
<div className="primitive">
Variants
</div>
<div className="example">

```cameligo group=variants
type action =
  Increment of int
| Decrement of int
```

</div>
<div className="primitive">
Variant *(pattern)* matching
</div>
<div className="example">

```cameligo group=variants
let a : action = Increment 5

let result : int =
  match a with
    Increment n -> n + 1
  | Decrement n -> n - 1
```

</div>
<div className="primitive">
Records
</div>
<div className="example">

```cameligo
type person = {
  age  : int;
  name : string
}

let john : person = {
  age  = 18;
  name = "john doe"
}

let name : string = john.name
```

</div>
<div className="primitive">
Maps
</div>
<div className="example">

```cameligo
type prices = (nat, tez) map

let prices : prices =
  Map.literal [
    (10n, 60mutez);
    (50n, 30mutez);
    (100n, 10mutez);
  ]

let price : tez option = Map.find_opt 50n prices

let prices : prices = Map.update 200n (Some 5mutez) prices
```

</div>
<div className="primitive">
Contracts & Accounts
</div>
<div className="example">

```cameligo group=tezos_specific
let destinationAddress : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)

let contract : unit contract =
  match (Tezos.get_contract_opt (Tezos.get_sender ()) : unit contract option) with
    Some contract -> contract
    | None -> (failwith "no contract" : unit contract)
```

</div>
<div className="primitive">
Transactions
</div>
<div className="example">

```cameligo group=tezos_specific

let payment : operation =
  Tezos.transaction unit 100mutez contract

```

</div>
<div className="primitive">
Exception/Failure
</div>
<div className="example">

```cameligo
let fail (u : unit) : unit =
  failwith "a failure message"
```

</div>
</div>

</Syntax>

<Syntax syntax="jsligo">

<div className="codeTable">
<div className="primitive">Strings</div>
<div className="example">

```jsligo
let name: string = "Tezos";
```

</div>
<div className="primitive">
Characters
</div>
<div className="example">

```jsligo
let t: string = "t";
```

</div>
<div className="primitive">
Integers
</div>
<div className="example">

```jsligo
let i: int = 42;
```

</div>
<div className="primitive">
Natural numbers
</div>
<div className="example">

```jsligo
let n: nat = 7 as nat;
```

</div>
<div className="primitive">
Unit
</div>
<div className="example">

```jsligo
let u: unit = unit;
```

</div>
<div className="primitive">
Boolean
</div>
<div className="example">

```jsligo
let has_drivers_license: bool = false
let adult: bool = true
```

</div>
<div className="primitive">
Boolean Logic
</div>
<div className="example">

```jsligo
let booleanLogic: bool =
    (!true) ==
    false ==
    (false && true) ==
    (false || false)
```

</div>
<div className="primitive">
Mutez (micro tez)
</div>
<div className="example">

```jsligo
let tez: tez = 42 as tez
let tez2: tez = 7 as mutez
```

</div>
<div className="primitive">
Address
</div>
<div className="example">

```jsligo
let tz1address: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;
let kt1address: address =
  "KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" as address;
```

</div>
<div className="primitive">
Addition
</div>
<div className="example">

```jsligo
let add_int: int = 3 + 4;
let add_nat: nat = (3 as nat) + (4 as nat);
```

</div>
<div className="primitive">
Multiplication & Division
</div>
<div className="example">

```jsligo
let mul_int: int = 3 * 4;
let mul_nat: nat = (3 as nat) * (4 as nat);

let div_int: int = 10 / 5;
let div_nat: nat = (10 as nat) / (5 as nat);
```

</div>
<div className="primitive">
Modulo
</div>
<div className="example">

```jsligo
let mod_nat: nat = 10 % 3
```

</div>
<div className="primitive">
Tuples
</div>
<div className="example">

```jsligo
type name = [string, string]

let winner: name = ["John", "Doe"]

let firstName: string = winner[0]
let lastName: string = winner[1]
```

</div>
<div className="primitive">
Types
</div>
<div className="example">

```jsligo group=b
type age = int
type name = string
```

</div>
<div className="primitive">
Includes
</div>
<div className="example">

```#include "library.jsligo"```

</div>
<div className="primitive">
Functions (short form)
</div>
<div className="example">

```jsligo
let add = (a: int, b: int): int =>
  a + b;
```

</div>
<div className="primitive">
Functions (long form)
</div>
<div className="example">

```jsligo group=b
let add = (a: int, b: int): int => {
  let c = a;
  let d = b;
  return c + d
};
```

</div>
<div className="primitive">
If Statement
</div>
<div className="example">

```jsligo
let if_statement = (age : nat): string => {
  if (age >= (16 as nat)) { return "yes"; } else { return "no"; }
}
```

</div>
<div className="primitive">
Options
</div>
<div className="example">

```jsligo
type middle_name = option<string>;
let middle_name : middle_name = Some("Foo");
let middle_name_ : middle_name = None();
```

</div>
<div className="primitive">
Variable Binding
</div>
<div className="example">

```jsligo
let age: int = 5
```

</div>
<div className="primitive">
Type Annotations
</div>
<div className="example">

```jsligo
let someAddress: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;
```

</div>
<div className="primitive">
Variants
</div>
<div className="example">

```jsligo group=variants
type action =
  ["Increment", int]
| ["Decrement", int];
```

</div>
<div className="primitive">
Variant *(pattern)* matching
</div>
<div className="example">

```jsligo group=variants
let a: action = Increment(5)
let result: int = match(a, {
  Increment: (n: int) => n + 1,
  Decrement: (n: int) => n - 1
})
```

</div>
<div className="primitive">
Records
</div>
<div className="example">

```jsligo
type person = {
  age: int,
  name: string
}

let john : person = {
  age: 18,
  name: "john doe"
}

let name_: string = john.name
```

</div>
<div className="primitive">
Maps
</div>
<div className="example">

```jsligo
type prices = map<nat, tez>;

let prices: prices = Map.literal(list([
  [10 as nat, 60 as mutez],
  [50 as nat, 30 as mutez],
  [100 as nat, 10 as mutez]
]));

let price: option<tez> = Map.find_opt(50 as nat, prices)

let prices2: prices = Map.update(200 as nat, (Some (5 as mutez)), prices)
```

</div>
<div className="primitive">
Contracts & Accounts
</div>
<div className="example">

```jsligo group=tezos_specific
let destinationAddress: address =
  "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" as address;

let contract : contract<unit> =
  match (Tezos.get_contract_opt(Tezos.get_sender()) as option<contract<unit>>, {
    Some: (contract: contract<unit>) => contract,
    None: () => (failwith("no contract") as contract<unit>)
  })
```

</div>
<div className="primitive">
Transactions
</div>
<div className="example">

```jsligo group=tezos_specific

let payment: operation =
  Tezos.transaction(unit, 100 as mutez, contract);

```

</div>
<div className="primitive">
Exception/Failure
</div>
<div className="example">

```jsligo
let fail = (u: unit) : unit =>
  failwith("a failure message")
```

</div>
</div>

</Syntax>

</div>
