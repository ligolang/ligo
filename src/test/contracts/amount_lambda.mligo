module Tezos = Tezos.Next

(* should return a constant function *)

let f1 () : unit -> tez =
  let amt : tez = Tezos.get_amount () in
  fun () -> amt

(* should return an impure function *)

let f2 () : unit -> tez = fun () -> Tezos.get_amount ()

[@entry]
let main (b : bool) (_ : unit -> tez) : operation list * (unit -> tez) =
  [], (if b then f1 () else f2 ())
