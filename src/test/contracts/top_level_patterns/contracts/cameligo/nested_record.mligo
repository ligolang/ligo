type t = { c : nat ; d : int ; e : string }
type r = { a : t   ; b : t   ; c : t      }

let r = { a = { c = 1n ; d = 1 ; e = "H" } 
        ; b = { c = 2n ; d = 2 ; e = "E" } 
        ; c = { c = 3n ; d = 3 ; e = "L" }
        }
let { a = { c = c1 ; d = d1 ; e = e1 }
    ; b = { c = c2 ; d = d2 ; e = e2 }
    ; c = { c = c3 ; d = d3 ; e = e3 } 
    } = r
let { a = { c = c4 ; d = d4 ; e = e4 }
    ; b = { c = c5 ; d = d5 ; e = e5 }
    ; c = { c = c6 ; d = d6 ; e = e6 } 
    }
  = { a = { c = 4n ; d = 4 ; e = "L" } 
    ; b = { c = 5n ; d = 5 ; e = "O" } 
    ; c = { c = 6n ; d = 6 ; e = "O" }
    }

type storage = nat * int * string

let main (_ : unit) (_ : storage) : operation list * storage
  = [], ( c1 + c2 + c3 + c4 + c5 + c6
        , d1 + d2 + d3 + d4 + d5 + d6
        , e1 ^ e2 ^ e3 ^ e4 ^ e5 ^ e6 )