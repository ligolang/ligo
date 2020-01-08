let main2 (p : key_hash) (s : unit) =
  let c : unit contract = Current.implicit_account p in 
  (([] : operation list), unit)

let main (t: key_hash * unit) = main2 t.0 t.1
