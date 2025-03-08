let main (p : int) (_s : unit) : operation list * unit =
  if p > 10 then failwith "Failure." else [], ()