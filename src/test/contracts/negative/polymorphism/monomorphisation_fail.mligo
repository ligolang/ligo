let f (_ : unit) s = ([], s)

[@entry]
let main (p : unit) (s : unit) : operation list * unit = f p s
