let p = []

let main (_ : int list) (_ : nat) : (operation list * nat) =
    let xs : int list    = [] in
    let ys : string list = [] in
    let a                = List.length xs in
    let b                = List.length ys in
    [], (a + b)