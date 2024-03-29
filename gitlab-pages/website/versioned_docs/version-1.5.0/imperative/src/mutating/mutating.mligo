let add (a, b : int * int) : int =
  let mut c = a + b in // Mutable c is assigned a + b
  let () = c := c + 1  // Reassignment of incremented c
  in c                 // c = a + b + 1