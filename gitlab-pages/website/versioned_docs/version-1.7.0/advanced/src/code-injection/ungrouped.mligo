let michelson_add n =
  [%Michelson ({| { UNPAIR ; ADD } |} : nat * nat -> nat)] n