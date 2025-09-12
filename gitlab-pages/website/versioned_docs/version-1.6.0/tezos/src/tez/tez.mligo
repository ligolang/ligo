let sum : tez = 5mutez + 0.000_010tez
let amount : tez option = 5mutez - 1mutez (* Some (4mutez) *)
let negative : tez option = 1mutez - 5mutez (* None *)
let mult : tez = 5n * 5mutez
let div : nat = 10mutez / 3mutez