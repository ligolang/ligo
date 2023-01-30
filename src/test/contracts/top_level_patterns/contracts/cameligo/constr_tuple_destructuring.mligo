type t = Foo of int

let (a, ((Foo x), y)) = 1, (Foo 2, "hey")

let main (_ : unit) (_ : int) : operation list * int
  = [], a + x + String.length y