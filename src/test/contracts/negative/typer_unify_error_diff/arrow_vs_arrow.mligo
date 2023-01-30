
let main (_p : int) (s : int) : operation list * int =
  let  x : int -> nat -> nat -> tez        = (fun _x _y _z -> 1tez) in
  let _y : int -> int -> int -> int -> nat = x in
  //              ^^^    ^^^    ^^^    ^^^
  ([] : operation list), s
