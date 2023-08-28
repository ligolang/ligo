let foo (type a) : a list -> a list =
  let id = fun (type b) (xs : b list) : b list -> xs in
  fun (xs : a list) : a list -> id xs

[@entry]
let main () (_ : int list) : operation list * int list = [], foo [1; 2; 3]
