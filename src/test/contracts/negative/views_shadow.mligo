type 'a return = operation list * 'a

[@view] let v1 (n,s: int * int) : int = s + n + 1
let v1 (n,s: int * int) : int = s + n + 111111

let main (((),s): unit * int) : int return = ([]:operation list) , s
