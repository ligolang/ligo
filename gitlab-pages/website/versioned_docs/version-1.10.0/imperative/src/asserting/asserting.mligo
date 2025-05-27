let incr_if_true (b : bool) (n : int) : int =
  let () = assert b in n+1

let incr_if_some (b : unit option) (n : int) : int =
  let () = assert_some b in n+1