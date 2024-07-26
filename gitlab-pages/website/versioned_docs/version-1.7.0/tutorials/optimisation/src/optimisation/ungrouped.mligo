let sum (x, y : int * int) = x + y

let main (parameter, storage : int * int) : operation list * int =
  ([], sum (parameter, storage))
let n = 4

let main (_, _ : unit * int) : operation list * int = [], n * n
type storage = { large_entrypoint : (bool, int -> int) big_map; result : int }

let load_large_ep (store : storage) : (int -> int) =
  let maybe_large_entrypoint =
    Big_map.find_opt true (store.large_entrypoint) in
  match maybe_large_entrypoint with
    Some ep -> ep
  | None -> failwith "Internal error"

[@entry]
let large_entry_point (n : int) (store :  storage) : operation list * storage =
  [], {store with result = (load_large_ep store) n}

(* Other entrypoints ... *)