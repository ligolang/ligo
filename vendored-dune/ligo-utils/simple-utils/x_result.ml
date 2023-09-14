open Core
include Result

(* Syntax *)
let ( let* ) x f = bind ~f x
