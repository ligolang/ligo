
(* In this case, one of the types is a tuple but not the other
   no diff should be displayed *)
let main (_p, s : int * int) : operation list * int =
  let  x = 42 in
  let _y : nat * int * string = x in
  ([] : operation list), s