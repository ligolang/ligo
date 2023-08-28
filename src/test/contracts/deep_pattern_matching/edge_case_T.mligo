type t =
| A
| B

type p = (t * t * t * t)

[@entry]
let main (p : p) (_ : int) : operation list * int =
  [],
  (match p with
     A, A, A, A -> 1
   | B, B, B, B -> 1
   | _, A, A, A -> 2
   | _, B, B, B -> 2
   | _, _, A, A -> 3
   | _, _, B, B -> 3
   | _, _, _, A -> 4
   | _, _, _, B -> 4)
