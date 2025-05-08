module Tezos = Tezos.Next

[@entry]
let main (p : int * int) (_ : unit) : operation list * unit =
  [Tezos.Operation.emit "%foo" p; Tezos.Operation.emit "%bar" p.0], ()
