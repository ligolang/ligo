type storage =
  {
    storage : int;
    dynamic_entrypoints;
  }

[@dyn_entry]
let one () (_ : int) : operation list * int = [], 1

[@dyn_entry]
let two : int ticket -> (int * int) -> operation list * (int * int) = [%external ("OPT_OUT_ENTRY")]

[@dyn_entry]
let three : unit -> int -> operation list * int = fun () _ -> [], 3

[@dyn_entry]
let four : (int,nat) entrypoint = [%external ("OPT_OUT_ENTRY")]

[@entry]
let nope () (_ : storage) : operation list * storage = failwith ()