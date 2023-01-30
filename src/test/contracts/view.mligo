type 'a return = operation list * 'a

[@view] let v1 (n : int) (s: int) : int = s + n + 1
let v2 (_ : int) (s: int) : int = s + 2
let bad_view (_ : int) (_: nat) : nat = 1n

let main (() : unit) (s : int) : int return = ([]:operation list) , s