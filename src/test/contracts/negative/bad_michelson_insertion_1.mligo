// Test michelson insertion in CameLIGO

let main (p, s : nat * nat ) : operation list * nat =
  let f : nat * nat -> nat = [%Michelson ({| ADD |} : nat * nat -> nat)]
  in [], f (p, s)
