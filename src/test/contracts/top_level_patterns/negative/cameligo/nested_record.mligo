type t = { c : nat ; d : int ; e : string }
type r = { a : t   ; b : t   ; c : t      }

let r = { a = { c = 1n ; d = 1 ; e = "H" } 
        ; b = { c = 2n ; d = 2 ; e = "E" } 
        ; c = { c = 3n ; d = 3 ; e = "L" }
        }
let { a = { c = c1 ; d = d1 ; e = e1 }
    ; b = { c = c2 ; d = d2 ; e = e2 }
    ; c = { c = c3 ; d = c1 ; e = e3 } 
    } = r