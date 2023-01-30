type 'a return = operation list * 'a

[@view] let v1 = fun (n : int) (s : int) : int -> s + n + 1
let v1 = fun (n : int) (s: int) : int -> s + n + 111111

let main (() : unit) (s : int) : int return = ([]:operation list) , s
