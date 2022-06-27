let main (_, s : unit * int) : operation list * int =
  let _ = if s > 10 then failwith "nyan was here" in
  (([] : operation list), s)
