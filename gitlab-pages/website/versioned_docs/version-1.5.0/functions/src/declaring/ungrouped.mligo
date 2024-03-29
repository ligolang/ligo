let drop x _ = x
let drop x _y = x  // _y silently ignored
let convoluted_doubling x =
  let add_x y = x + y  // x is bound by convoluted_doubling
  in add_x x