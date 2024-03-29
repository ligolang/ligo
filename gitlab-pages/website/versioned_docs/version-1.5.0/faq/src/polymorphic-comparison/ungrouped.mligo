[@inline] let compare_equal (type k) (a : k) (b : k) =
  [%Michelson ({|{ UNPAIR; COMPARE; EQ }|} : k * k -> bool)] (a, b)