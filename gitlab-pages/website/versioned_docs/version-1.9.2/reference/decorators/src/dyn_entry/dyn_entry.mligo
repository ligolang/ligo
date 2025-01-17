type storage = {
  storage : int;
  dynamic_entrypoints;
}

[@dyn_entry]
let one () (_ : int) : operation list * int = [], 1

[@dyn_entry]
let tick : int ticket -> int * int -> operation list * (int * int) =
  fun _ p -> ([], p)