type storage = unit

(* not supported yet
let main (p:unit) storage =
  (fun x -> ()) ()
*)

let main (ps: unit * storage) = (fun (_: unit) -> ()) ()
