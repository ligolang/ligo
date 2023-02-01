let main (_, s : unit * int) : operation list * int =
  let a = Some 42 in
  let b =
    match a with
    | Some x -> x + s
    | None -> s
  in (([] : operation list), b)
