let y = -(-1) (* In CameLIGO *)
(* In CameLIGO *)
let res = 
  type t = int list in
  let x : t = [42] in
  43 :: x
type storage = string
type result = operation list * storage

[@entry] let hello (_u : unit) (_store : storage) : result =
  [], "hello"

[@entry] let big (_u : unit) (store : storage) : result =
  [], store ^ " big"

type planet = Earth | Mars | Earth2

[@view] let world (p : planet) (store : storage) : string =
  let world = match p with
    | Earth -> " pale blue dot"
    | Mars -> " pale red dot"
    | Earth2 -> failwith "backup planet not found"
  in
  store ^ " " ^ world