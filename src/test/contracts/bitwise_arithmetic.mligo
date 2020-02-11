(* Test CameLIGO bitwise operators *)

let or_op  (n: nat) : nat = Bitwise.lor  n 4n
let and_op (n: nat) : nat = Bitwise.land n 7n
let xor_op (n: nat) : nat = Bitwise.lxor n 7n
let lsl_op (n: nat) : nat = Bitwise.shift_left n 7n
let lsr_op (n: nat) : nat = Bitwise.shift_right n 7n
