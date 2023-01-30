type storage = unit

(* not supported yet
let main (p, s : unit * storage) = (fun x -> ()) ()
*)

let main (_ : unit) (_ : storage) = (fun (_ : unit) -> ()) ()
