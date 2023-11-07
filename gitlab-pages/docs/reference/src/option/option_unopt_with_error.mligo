
let v_opt : int option = Some 1

let v : int = Option.unopt_with_error v_opt "FooBar" (* 1 *)

let none : int = Option.unopt_with_error (None : int option) "FooBar" (* fails with "FooBar" *)
