type storage = unit

(* let%entry main (p:unit) storage = *)
(*   (failwith "This contract always fails" : unit) *)

let%entry main (p:unit) storage =
  if true then failwith "This contract always fails" else ()

