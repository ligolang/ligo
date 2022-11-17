let f (_ : unit) s = ([], s)

let main ((p, s) : unit * unit) : operation list * unit = f p s
