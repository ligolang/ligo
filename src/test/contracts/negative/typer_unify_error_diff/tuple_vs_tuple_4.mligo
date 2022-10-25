
(*
  Yet another example, with longer tuples.
*)



type a = int *                nat * int * nat     * int *       nat
type b = int * tez * string * nat * int * address * int * tez * nat
//             ^^^   ^^^^^^               ^^^^^^^         ^^^

let main (_p, s : int * int) : operation list * int =
  let x  : a = 42 , 4n , 42 , 24n , 42 , 24n in
  let _y : b = x in
  ([] : operation list), s