let magic (type a) (x : unit) : a option = failwith ()

let test (type a) : unit = [%external ("UNOPT", magic ())] (1, ())