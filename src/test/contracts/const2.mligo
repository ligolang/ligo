let const = fun (type a b) (a : a) (_ : b) : a -> a
 
let main (_ : unit * unit) : operation list * unit =
  [], const () 0
