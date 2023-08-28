type t = Foo of int
type r = { a : int ; b : t ; c : string }

let { a ; b = (Foo x) ; c} = { a = 1 ; b = Foo 2 ; c = "hey" }

[@entry]
let main (_ : unit) (_ : int) : operation list * int
  = [], a + x + String.length c
