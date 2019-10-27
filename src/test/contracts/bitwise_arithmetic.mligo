(* Test CameLIGO bitwise operators *)

let or_op (n : nat) : nat =
  Bitwise.lor n 4p

let and_op (n : nat) : nat =
  Bitwise.land n 7p

let xor_op (n : nat) : nat =
  Bitwise.lxor n 7p
