let main (p : key_hash) (s : unit) =
  let c : unit contract = Current.implicit_account p in 
  (([] : operation list), unit)