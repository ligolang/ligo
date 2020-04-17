// Test michelson insertion in CameLIGO

let michelson_add (n : nat) : nat =
  let f : nat -> nat = [%Michelson ({| DUP;ADD |} : nat -> nat) ] in
  f n
