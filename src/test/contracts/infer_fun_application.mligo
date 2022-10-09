let magic (type a) (x : unit) : a option = failwith ()

let test (type a) : unit = (Option.unopt (magic ())) (1, ())
