let const = fun (type a b) (a, _ : a * b) : a -> a
 
let main (_ : unit * unit) : operation list * unit =
  [], const ((), 0)
