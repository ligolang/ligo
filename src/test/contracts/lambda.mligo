type storage = unit

(* not supported yet
let main (p:unit) storage =
  (fun x -> ()) ()
*)

let main (p: unit) storage = (fun (_: unit) -> ()) ()
