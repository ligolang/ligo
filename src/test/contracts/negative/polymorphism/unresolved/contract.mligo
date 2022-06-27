let main (_, _ : int list * nat) : (operation list * nat) =
    let xs : int list    = [] in
    let ys : string list = [] in
    let a                = List.length xs in
    let b                = List.length ys in
    [], (a + b + List.length [])