type storage = unit

(* not supported yet
let%entry main (p:unit) storage =
  (fun x -> ()) ()
*)

let%entry main (p:unit) storage =
  (fun (f : unit -> unit) -> f ())
    (fun (x : unit) -> unit)
