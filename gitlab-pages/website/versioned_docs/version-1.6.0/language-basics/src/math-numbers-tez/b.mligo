let a : int = 5 - 10

// Subtraction of two nats yields an int
let b : int = 5n - 2n

// Therefore the following is invalid
// let c : nat = 5n - 2n
let d : tez option = 5mutez - 1mutez (* Some (4mutez) *)
let e : tez option = 1mutez - 5mutez (* None *)