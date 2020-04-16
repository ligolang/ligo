// Test michelson insertion in ReasonLIGO

let michelson_add = (n : nat) : nat =>
  [%Michelson {| DUP;ADD |} : nat => nat ]
