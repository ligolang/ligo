let a : int = 37
let b : int = 5
let ediv1 : (int * nat) option = ediv a b  // Some (7, 2)
let c : nat = 37n
let ediv2 : (int * nat) option = ediv c b  // Some (7, 2)
let d : nat = 5n
let ediv3 : (nat * nat) option = ediv c d  // Some (7, 2)
let ediv4 : (int * nat) option = ediv a d  // Some (7, 2)