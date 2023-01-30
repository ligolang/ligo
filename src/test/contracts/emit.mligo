let main (p : int * int) (_ : unit) : operation list * unit =
  [Tezos.emit "%foo" p ; Tezos.emit "%bar" p.0],()