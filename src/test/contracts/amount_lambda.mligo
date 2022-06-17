(* should return a constant function *)
let f1 (x : unit) : unit -> tez =
  let amt : tez = Tezos.get_amount () in
  fun (x : unit) -> amt

(* should return an impure function *)
let f2 (x : unit) : unit -> tez =
  fun (x : unit) -> Tezos.get_amount ()

let main (b,s : bool * (unit -> tez)) : operation list * (unit -> tez) =
  (([] : operation list), (if b then f1 () else f2 ()))
