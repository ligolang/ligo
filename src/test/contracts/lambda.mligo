type storage = unit

(* not supported yet
let main (p, s : unit * storage) = (fun x -> ()) ()
*)

let check (_ : unit) (_ : storage) = (fun (_ : unit) -> ()) ()
