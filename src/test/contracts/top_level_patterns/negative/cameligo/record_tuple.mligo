type r = { a : nat    * int * string
         ; b : int    * nat * string 
         ; c : string * int * nat
         }

let r = { a = (1n     , 1 , "H") 
        ; b = (2      , 2n, "E") 
        ; c = ("Hello", 3 , 3n ) 
        }
let { a = (a1, a2, a3)
    ; b = (b1, a2, b3)
    ; c = (c1, c2, c3)
    } = r