type sum_with_record =
  | A of { a1 : int ; a2 : string }
  | B of { b1 : string ; b2 : int }

let example = A { a1 = 42 ; a2 = "meow" }

[@entry]
let main () (s : int) : operation list * int =
  let res =
    match example with
    | A inner -> inner.a1
    | B inner -> inner.b2
  in (([] : operation list), s + res)
