let add (a, b : int * int) : int = a + b

let someCheck(a, b : int * int) =
  if a + b < 10
  then failwith "bruh"
  else ()

[@entry]
let main () (s : int) : operation list * int = begin
  let a = s + 2 in
  someCheck(10, 15);
  let b = add(a, s + a) in let c = 1000 - 7 in
  (([] : operation list), a + b + c)
  end
