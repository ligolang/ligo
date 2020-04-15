// Test michelson insertion in PascaLIGO

function michelson_add (var n : nat) : nat is
  [%Michelson {| DUP;ADD |} : nat -> nat ]
