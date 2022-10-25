
(* In this case, the types are not record types,
   no diff should be displayed *)
let main (_p, s : int * int) : operation list * int =
  let  x : int = 42 in
  let _y : nat = x in
  ([] : operation list), s