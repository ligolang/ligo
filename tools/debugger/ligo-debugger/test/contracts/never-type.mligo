let never_value () : never = failwith "What did you expected? It's a bottom!"

let main (_, s : unit * int) : operation list * int =
  let val = never_value() in
  (([] : operation list), s)
