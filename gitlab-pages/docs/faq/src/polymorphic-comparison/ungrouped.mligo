[@inline] let equal (type a) (val_a : a) (val_b : a) =
  [%Michelson ({|{ UNPAIR; COMPARE; EQ }|} : a * a -> bool)] (val_a, val_b)