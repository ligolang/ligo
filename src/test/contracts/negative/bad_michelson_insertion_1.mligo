// Test michelson insertion in CameLIGO

[@entry]
let main (p : nat) (s : nat) : operation list * nat =
  let f : nat * nat -> nat = [%Michelson ({| ADD |} : nat * nat -> nat)] in
  [], f (p, s)
