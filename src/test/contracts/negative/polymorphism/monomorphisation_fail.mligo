let f (_ : unit) s = ([], s)

let main (p : unit) (s : unit) : operation list * unit = f p s
