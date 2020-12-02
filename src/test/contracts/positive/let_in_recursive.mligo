(* example from https://gitlab.com/ligolang/ligo/-/issues/1066 *)

let rec ref_join
  (params: int * (int -> int -> int))
  : int * int =
  let left = 1 in
  ref_join
    ( 1
    , (fun (mem: int) (new_: int) ->
        if left > 0 then 0 else left+1)
    )

let main (ign: unit * unit): operation list * unit =
  let ign = ref_join (1, (fun (m: int) (r: int) -> 0)) in
  ([] : operation list), ()
