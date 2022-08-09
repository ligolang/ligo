let main (p,_ : (int*int) * unit ) : operation list * unit =
  [Tezos.emit "%foo" p ; Tezos.emit "%bar" p.0],()