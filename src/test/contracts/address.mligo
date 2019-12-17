let main (p : key_hash) =
  let c : unit contract = Current.implicit_account p in 
  Current.address c