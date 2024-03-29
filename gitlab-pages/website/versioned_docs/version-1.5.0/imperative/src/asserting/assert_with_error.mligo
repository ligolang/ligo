let incr_if_true (b : bool) (n : int) : int =
  let () = assert_with_error b "My custom error message."
  in n+1