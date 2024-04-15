let name : string = "Tezos"
let t : string = "t"
let i : int = 42
let n : nat = 7n
let u : unit = unit
let has_drivers_license : bool = false
let adult : bool = true
let booleanLogic : bool =
    (not true) =
    false =
    (false && true) =
    (false || false)
let tez : tez = 42tez
let tez : tez = 7mutez
let tz1address : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
let kt1address : address =
  ("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD" : address)
let my_str : string = "Hello World!"
let verbatim_str : string = {|verbatim string|}
let add_int : int = 3 + 4
let add_nat : nat = 3n + 4n
let mul_int : int = 3 * 4
let mul_nat : nat = 3n * 4n

let div_int : int = 10 / 5
let div_nat : nat = 10n / 5n
let mod_nat : nat = 10 mod 3
type name = string * string

let winner : name = "John", "Doe"

let firstName : string = winner.0
let lastName : string = winner.1
type age = int
type name = string
let add (a : int) (b : int) : int =
  a + b
let can_drive (age : nat) : string =
  if age >= 16n then "yes" else "no"
type middle_name = string option
let middle_name : middle_name = Some "Foo"
let middle_name : middle_name = None
let age : int = 5
let someAddress : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
type person = {
  age  : int;
  name : string
}

let john : person = {
  age  = 18;
  name = "john doe"
}

let name : string = john.name
type prices = (nat, tez) map

let prices : prices =
  Map.literal [
    (10n, 60mutez);
    (50n, 30mutez);
    (100n, 10mutez);
  ]

let price : tez option = Map.find_opt 50n prices

let prices : prices = Map.update 200n (Some 5mutez) prices
let fail (u : unit) : unit =
  failwith "a failure message"
type animal =
[@layout comb]
| Elephant
| Dog
| Cat
type animal =
[@layout tree]
| Elephant
| Dog
| Cat
module FA0_inferred = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end
module type FA0_SIG = sig
  type t
  [@entry] val transfer : unit -> t -> operation list * t
end
module type FA0Ext_SIG = sig
  include FA0_SIG
  [@entry] val transfer2 : unit -> t -> operation list * t
end
module FA0 : FA0_SIG = struct
  type t = unit
  [@entry] let transfer (_ : unit) (_ : t) : operation list * t = [], ()
end
module FA0Ext : FA0Ext_SIG = struct
  include FA0
  [@entry] let transfer2 (a : unit) (b : t) = transfer a b
end