let const = fun (type a b) (a, _ : a * b) : a -> a

[@entry]
let main (_ : unit) (_ : unit) : operation list * unit = [], const ((), 0)
