let incr_if_true (b : bool) (n : int) : int =
  let () = Assert.Error.assert b "My custom error message."
  in n+1