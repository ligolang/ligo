[@entry]
let main () (s : int option) : operation list * int option =
  let s2 = match s with
    | Some x -> x + 1
    | None -> 0
    in
  (([] : operation list), Some s2)
