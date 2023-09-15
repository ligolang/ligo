type storage =
  {
    storage : int;
    dynamic_entrypoints;
  }

[@dyn_entry]
let one () (_ : int) : operation list * int = [], 1

[@dyn_entry]
let tick : int ticket -> (int * int) -> operation list * (int * int) =
  fun _ x -> [], x

[@entry]
let call_one () (s : storage) : operation list * storage =
  match Dynamic_entrypoints.get one s.dynamic_entrypoints with
    Some f ->
      let op, storage = f () s.storage in
      op, {s with storage}
  | None -> failwith (-1)
  
[@entry]
let call_tick (p : int ticket) (s : storage) : operation list * storage =
  match Dynamic_entrypoints.get tick s.dynamic_entrypoints with
    Some f ->
      let op, (i1, i2) = f p (s.storage, 0) in
      op, {s with storage = i1 + i2}
  | None -> failwith (-1)

[@entry]
let set_one (one_v2 : (unit,int) entrypoint) (s : storage) : operation list * storage =
  let dynamic_entrypoints =
    Dynamic_entrypoints.set one (Some one_v2) s.dynamic_entrypoints in
  [], {s with dynamic_entrypoints}
