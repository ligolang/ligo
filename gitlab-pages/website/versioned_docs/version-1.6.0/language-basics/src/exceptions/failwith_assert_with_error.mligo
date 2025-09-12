[@entry]
let main (p : bool) (s : unit) : operation list * unit =
  let () = assert_with_error p "My custom error message."
  in [], s