let x =
  module A = struct
    let (x, x) = (1, "1")
  end in
  A.x ^ "1"
