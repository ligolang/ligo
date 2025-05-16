let magic (type a) (x : unit) : a option = failwith ()

let test (type a) : unit =
  (Option.value_with_error "option is None" (magic ())) (1, ())
