type 'a return = operation list * 'a

let v1 (_: int * string) : string = "YOU SHOULD NOT SEE THAT"
[@view] let v1 (n,s: int * int) : int = s + n + 1


let main (((),s): unit * int) : int return = ([]:operation list) , s
