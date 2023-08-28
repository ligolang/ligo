let to_tup (x : int) : (string * int) = ("foo", x)

let to_int ((_, x) : (string * int)) : int = x

[@entry]
let main () (store : int option) : operation list * (int option) =
  ([] : operation list), Option.map to_int (Option.map to_tup store)

let test =
  let a = Some 1 in
  let b = Option.map to_tup a in
  let _ = assert (b = (Some ("foo", 1))) in
  let _ = assert (a = (Option.map to_int b)) in
  let a : int option = None in
  let b = Option.map to_tup a in
  let _ = assert (b = (None : (string * int) option)) in
  let _ = assert (a = (Option.map to_int b)) in
  ()
