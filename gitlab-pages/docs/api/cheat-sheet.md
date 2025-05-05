---
id: cheat-sheet
title: Cheat Sheet
---

import Syntax from '@theme/Syntax';

<div className="cheatsheet">

<Syntax syntax="cameligo">

<div className="codeTable">
<div className="primitive">
Contract, view and test
</div>
<div className="example">

```cameligo group=simple_contract_with_view_and_test
module Test = Test.Next

module C = struct
  type storage = int

  [@entry]
  let increment (action: int) (store: storage) : operation list * storage =
    [], store + action

  [@entry]
  let decrement (action: int) (store: storage) : operation list * storage =
    [], store - action

  [@view]
  let get_storage (must_be_positive: bool) (storage: int) : int =
    if must_be_positive && storage < 0 then
      failwith "Negative value in storage"
    else
      storage
end

let testC =
  let initial_storage = 42 in
  let originated =
    Test.Originate.contract (contract_of C) initial_storage 0tez in
  let p : C parameter_of = Increment 1 in
  let _ = Test.Typed_address.transfer_exn originated.taddr p 1mutez in
  Assert.assert (Test.get_storage originated.taddr = initial_storage + 1)
```

</div>
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
String
</div>
<div className="example">

```cameligo
let my_str : string = "Hello World!"
```

</div>
<div className="primitive">
Verbatim string
</div>
<div className="example">

```cameligo
let verbatim_str : string = {|verbatim string|}
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
Include (prefer import)
</div>
<div className="example">

```cameligo skip
#include "library.mligo"
```

</div>
<div className="primitive">
Import (better)
</div>
<div className="example">

```cameligo skip
#import "library.mligo" "MyLibrary"
let foo = MyLibrary.bar
```

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
module Tezos = Tezos.Next

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
  Tezos.Operation.transaction unit 100mutez contract

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
<div className="primitive">
Comb layout (default)
</div>
<div className="example">

```cameligo
type animal =
[@layout comb]
| Elephant
| Dog
| Cat
```

</div>
<div className="primitive">
Tree layout
</div>
<div className="example">

```cameligo
type animal =
[@layout tree]
| Elephant
| Dog
| Cat
```

</div>
<div className="primitive">
Module definition (auto-inferred type)
</div>
<div className="example">

```cameligo
module FA0_inferred = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end
```

</div>
<div className="primitive">
Module Type
</div>
<div className="example">

```cameligo
module type FA0_SIG = sig
  type t
  [@entry] val transfer : unit -> t -> operation list * t
end
```

</div>
<div className="primitive">
Extending Module Type
</div>
<div className="example">

```cameligo
module type FA0Ext_SIG = sig
  include FA0_SIG
  [@entry] val transfer2 : unit -> t -> operation list * t
end
```

</div>
<div className="primitive">
Module definition
</div>
<div className="example">

```cameligo
module FA0 : FA0_SIG = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end
```

</div>
<div className="primitive">
Extending module definition
</div>
<div className="example">

```cameligo
module FA0Ext : FA0Ext_SIG = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end
```

</div>
</div>

</Syntax>

<Syntax syntax="jsligo">

<div className="codeTable">
<div className="primitive">
Contract, view and test
</div>
<div className="example">

```jsligo group=simple_contract_with_view_and_test
import Test = Test.Next;

type storage = int;

class C {
  @entry
  increment = (action: int, store: storage) : [list <operation>, storage] => [[], store + action];

  @entry
  decrement = (action: int, store: storage) : [list <operation>, storage] => [[], store - action];

  @view
  get_storage = (must_be_positive: bool, storage: int): int => {
    if (must_be_positive && storage < 0)
      return failwith("Negative value in storage");
    else
      return storage;
  }
};

const testC = (() => {
  const initial_storage = 42;
  const originated = Test.Originate.contract(contract_of(C), initial_storage, 0 as tez);
  const p : parameter_of<C> = ["Increment" as "Increment", 1];
  Test.Typed_address.transfer_exn(originated.taddr, p, 1 as mutez);
  return Assert.assert(Test.Typed_address.get_storage(originated.taddr) == initial_storage + 1);
})()
```

</div>
<div className="primitive">Strings</div>
<div className="example">

```jsligo
const name: string = "Tezos";
```

</div>
<div className="primitive">
Characters
</div>
<div className="example">

```jsligo
const t: string = "t";
```

</div>
<div className="primitive">
Integers
</div>
<div className="example">

```jsligo
const i: int = 42;
```

</div>
<div className="primitive">
Natural numbers
</div>
<div className="example">

```jsligo
const n: nat = 7 as nat;
```

</div>
<div className="primitive">
Unit
</div>
<div className="example">

```jsligo
const u: unit = unit;
```

</div>
<div className="primitive">
Boolean
</div>
<div className="example">

```jsligo
const has_drivers_license: bool = false
const adult: bool = true
```

</div>
<div className="primitive">
Boolean Logic
</div>
<div className="example">

```jsligo
const booleanLogic: bool =
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
const tez_amount: tez = 42 as tez
const tez_amount2: tez = tez_amount + (7 as mutez) // == (42000007 as mutez)
```

</div>
<div className="primitive">
Address
</div>
<div className="example">

```jsligo
const tz1address: address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
const kt1address: address = "KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD";
```

</div>
<div className="primitive">
String
</div>
<div className="example">

```jsligo
const my_str : string = "Hello World!";
```

</div>
<div className="primitive">
Verbatim string
</div>
<div className="example">

```jsligo
const verbatim_str : string = `verbatim string`;
```

</div>
<div className="primitive">
Addition
</div>
<div className="example">

```jsligo
const add_int: int = 3 + 4;
const add_nat: nat = (3 as nat) + (4 as nat);
```

</div>
<div className="primitive">
Multiplication & Division
</div>
<div className="example">

```jsligo
const mul_int: int = 3 * 4;
const mul_nat: nat = (3 as nat) * (4 as nat);

const div_int: int = 10 / 5;
const div_nat: nat = (10 as nat) / (5 as nat);
```

</div>
<div className="primitive">
Modulo
</div>
<div className="example">

```jsligo
const mod_nat: nat = 10 % 3; // can fail (division by zero), check your inputs first.
```

</div>
<div className="primitive">
Tuples
</div>
<div className="example">

```jsligo
type name = [string, string];

const winner: name = ["John", "Doe"];

const firstName: string = winner[0];
const lastName: string = winner[1];
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
Include (prefer import)
</div>
<div className="example">
</div>
<div className="primitive">
Import (better)
</div>
<div className="example">

```jsligo skip
```

</div>
<div className="primitive">
Functions (short form)
</div>
<div className="example">

```jsligo
const add = (a: int, b: int): int =>
  a + b;
```

</div>
<div className="primitive">
Functions (long form)
</div>
<div className="example">

```jsligo group=b
function add (a: int, b: int): int {
  const c = a;
  const d = b;
  return c + d
};
```

</div>
<div className="primitive">
If/else Statement
</div>
<div className="example">

```jsligo
function if_statement (age : nat): string {
  if (age >= (16 as nat)) return "yes"; else return "no";
}
```

</div>
<div className="primitive">
Options
</div>
<div className="example">

```jsligo
type middle_name = option<string>;
const a_middle_name : middle_name = ["Some" as "Some", "Foo"];
const no_middle_name : middle_name = ["None" as "None"];
```

</div>
<div className="primitive">
Variable Binding
</div>
<div className="example">

```jsligo
const age: int = 5
```

</div>
<div className="primitive">
Type Annotations
</div>
<div className="example">

```jsligo
const someAddress: address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
```

</div>
<div className="primitive">
Variants (label + optional value)
</div>
<div className="example">

```jsligo group=variants
type action =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];
```

</div>
<div className="primitive">
Matching on variant cases
</div>
<div className="example">

```jsligo group=variants
const a: action = ["Increment" as "Increment", 5];
const result: int =
  $match(a, {
    "Increment": n => n + 1,
    "Decrement": n => n - 1,
    "Reset": () => 0
  });
```

</div>
<div className="primitive">
Records / Plain Old Data Objects
</div>
<div className="example">

```jsligo
type person = {
  age: int,
  name: string
}

const john : person = {
  age: 18,
  name: "john doe"
}

const name_: string = john.name
```

</div>
<div className="primitive">
Maps
</div>
<div className="example">

```jsligo
type prices = map<nat, tez>;

const prices: prices = Map.literal([
  [10 as nat, 60 as mutez],
  [50 as nat, 30 as mutez],
  [100 as nat, 10 as mutez]
]);

const price: option<tez> = Map.find_opt(50 as nat, prices)

const prices2: prices =
  Map.update(200 as nat, ["Some" as "Some", 5 as mutez], prices)
```

</div>
<div className="primitive">
Contracts & Accounts
</div>
<div className="example">

```jsligo group=tezos_specific
import Tezos = Tezos.Next;

const destinationAddress: address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

const contract : contract<unit> =
  $match(Tezos.get_contract_opt(Tezos.get_sender()) as option<contract<unit>>, {
    "Some": contract => contract,
    "None": () => failwith("no contract or wrong contract type")
  })
```

</div>
<div className="primitive">
Transactions
</div>
<div className="example">

```jsligo group=tezos_specific
const payment: operation =
  Tezos.Operation.transaction(unit, 100 as mutez, contract);
```

</div>
<div className="primitive">
Exception/Failure
</div>
<div className="example">

```jsligo
const fail = (u: unit) : unit =>
  failwith("a failure message")
```

</div>
<div className="primitive">
Comb layout (default)
</div>
<div className="example">

```jsligo
type animal =
// @layout("comb")
| ["Elephant"]
| ["Dog"]
| ["Cat"];
```

</div>
<div className="primitive">
Tree layout
</div>
<div className="example">

```jsligo
type animal =
// @layout("tree")
| ["Elephant"]
| ["Dog"]
| ["Cat"];
```

</div>
<div className="primitive">
Namespace (auto-inferred type)
</div>
<div className="example">

```jsligo
namespace FA0_inferred {
  type storage = int;
  // @entry
  const add = (s : int, k : int) : [list<operation>, int] => [[], s + k];
  // @entry
  const extra = (s : int, k : int) : [list<operation>, int] => [[], s - k];
}
```

</div>
<div className="primitive">
Interface
</div>
<div className="example">

```jsligo
type storage = int;

interface FA0_INTF {
  //@entry
  add : (s : int, k : storage) => [list<operation>, storage];
}
```

</div>
<div className="primitive">
Extending an interface
</div>
<div className="example">

```jsligo
interface FA0_EXT_INTF extends FA0_INTF {
  // @entry
  add : (s : int, k : storage) => [list<operation>, storage];
}
```

</div>
<div className="primitive">
Implementing an interface
</div>
<div className="example">

```jsligo
class FA0 implements FA0_INTF {
  @entry
  add = (s : int, k : int) : [list<operation>, int] => [[], s + k];

  @entry
  extra = (s : int, k : int) : [list<operation>, int] => [[], s - k];
}
```

</div>
</div>

</Syntax>

</div>

<!-- updated use of entry -->
