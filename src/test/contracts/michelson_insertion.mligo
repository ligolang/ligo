// Test michelson insertion in CameLIGO

let michelson_add (n : nat) : nat =
  let f : nat -> nat = [%Michelson {| DUP;ADD |}] in
  f n
