type t = A | B

type p = (t * t * t * t * t * t * t * t)

let main (p : p) (_ : int) : operation list * int =
  [], (match p with
    A, A, _, _, _, _, _, _ -> 1
  | _, _, A, A, _, _, _, _ -> 2
  | _, _, _, _, A, A, _, _ -> 3
  | _, _, _, _, _, _, A, A -> 4
  | A, B, A, B, A, B, A, B -> 5)