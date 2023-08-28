type 'a return = operation list * 'a

let v1 (_ : int) (_ : string) : string = "YOU SHOULD NOT SEE THAT"

[@view]
let v1 (n : int) (s : int) : int = s + n + 1

[@entry]
let main (() : unit) (s : int) : int return = ([] : operation list), s
