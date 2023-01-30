type t = A | B

type p = (t * t * t * t * t * t)

let main (p : p) (_ : int) : operation list * int = 
  [], (match p with 
    A,A,A,_,_,_ -> 1
  | B,_,_,A,A,_ -> 2
  | _,B,_,B,_,A -> 3
  | _,_,B,_,B,B -> 4)