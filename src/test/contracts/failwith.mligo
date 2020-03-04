type storage = unit

let main (p: unit) (s:storage) =
  if true then failwith "This contract always fails" else ()
