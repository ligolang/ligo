let id (type a) (x : a) : a = x

let main (_ : unit) (n : int) : operation list * int =
  ([] : operation list), id n
