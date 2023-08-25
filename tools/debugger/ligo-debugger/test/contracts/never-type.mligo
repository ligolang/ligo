let never_value () : never = failwith "What did you expected? It's a bottom!"

[@entry]
let main () (s : int) : operation list * int =
  let v = never_value() in
  (([] : operation list), s)
