type storage =
  [@layout:comb] {
  token_id                     : nat;
  [@annot] ignore_annotation  : address list
}

let main (_, s : unit * storage) : operation list * storage = [], s
