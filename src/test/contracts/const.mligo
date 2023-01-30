let const = fun (type a b) (a, b : a * b) : a -> a
 
let main (_ : unit) (_ : unit) : operation list * unit =
  [], const ((), 0)
