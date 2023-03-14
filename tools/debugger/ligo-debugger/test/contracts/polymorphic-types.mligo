let main (_, s : unit * int) : operation list * int =
  let s1_opt : int option = None in
  let s2_opt : string option = None in
  let s1 =
    match s1_opt with
    | None -> 0
    | Some x -> x in
  let s2 =
    match s2_opt with
    | None -> 0n
    | Some str -> String.length str in
  (([] : operation list), s + s1 + s2)
