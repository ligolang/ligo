
(*
  When two mismatching tuples are within lists,
  here [tuple_a list] vs [tuple_b list]
  the error should target the tuples themselves :
    cannot unify [tuple_a] with [tuple_b]
  and not the whole list types :
    cannot unify [tuple_a list] with [tuple_b list]
*)

type a = (string * int * nat * int *       string * int) list
type b = (tez    * int       * tez * nat * string)       list

let main (_p, s : int * int) : operation list * int =
  let  x = [ "foo" , 42  , 24n , 42 ,        "bar",  42 ] in
  let _y : b = x in
  ([] : operation list), s
