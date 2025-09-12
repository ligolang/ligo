
let v_opt : int option = Some 1

let v : int = Option.unopt v_opt (* 1 *)

let none : int = Option.unopt (None : int option) (* fails with "option is None" *)
