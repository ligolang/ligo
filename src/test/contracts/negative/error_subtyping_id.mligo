[@entry]
let main (_p: unit) (_s : unit) : operation list * unit =
  let f : int * bool -> string * bool = fun x -> x in
  ([], ())