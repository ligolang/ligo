type storage = unit

(* Not supported yet:
let main (p:unit) storage = (fun x -> ()) ()
*)

let main (_: unit) storage =
  (fun (f: unit -> unit) -> f ()) (fun (_: unit) -> unit)
