(* Test CameLIGO bitwise operators *)

let or_op (n : nat) : nat =
  Bitwise.bor n 4p

let and_op (n : nat) : nat =
  Bitwise.band n 7p

let xor_op (n : nat) : nat =
  Bitwise.xor n 7p
