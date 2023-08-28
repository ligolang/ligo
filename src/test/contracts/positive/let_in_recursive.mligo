(* example from https://gitlab.com/ligolang/ligo/-/issues/1066 *)

let rec ref_join
  (_params: int * (int -> int -> int))
  : int * int =
  let left = 1 in
  ref_join
    ( 1
    , (fun (_mem: int) (_new: int) ->
        if left > 0 then 0 else left+1)
    )

[@entry]
let main (_ : unit) (_ : unit): operation list * unit =
  let _ = ref_join (1, (fun (_m: int) (_r: int) -> 0)) in
  ([] : operation list), ()
