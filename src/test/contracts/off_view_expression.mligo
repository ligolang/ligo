let maina (p : int) (s : int) : operation list * int = [], p + s
let mainb (p : int) (s : nat) : operation list * nat = [], abs p + s

let va (p : string) (s : int) : int = String.length p + s
let vb (p : string) (s : nat) : int = int (String.length p + s)

