type param =
  | Noop
  | Inner

[@entry]
let main (p : param) (s : int * int) : operation list * (int * int) =
  match p with
  | Noop -> [], s
  | Inner -> [], s
