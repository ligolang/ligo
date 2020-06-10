let int_ (a : int) = a < a

let nat_ (a : nat) = a < a

let bool_ (a : bool) = a < a

let mutez_ (a : tez) = a < a

let string_ (a : string) = a < a

let bytes_ (a : bytes) = a < a

let address_ (a : address) = a < a

let timestamp_ (a : timestamp) = a < a

let key_hash_ (a : key_hash) = a < a

type comp_pair = int * int

let comp_pair (a : comp_pair) = a < a

type inner_record = (int, "one", nat, "two") michelson_pair

type comb_record =
  (int, "three", inner_record, "four") michelson_pair

let comb_record (a : comb_record) = a < a
