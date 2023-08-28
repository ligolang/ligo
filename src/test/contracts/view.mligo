type 'a return = operation list * 'a

[@view]
let v1 (n : int) (s : int) : int = s + n + 1

[@view]
let v2 (_ : int) (s : int) : int = s + 2

[@entry]
let main (() : unit) (s : int) : int return = ([] : operation list), s
