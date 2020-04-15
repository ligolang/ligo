// Test michelson insertion in ReasonLIGO

let michelson_add = (n : nat) : nat =>
  [%Michelson {| DUP;ADD; PUSH "hello" |} : nat => nat ]
