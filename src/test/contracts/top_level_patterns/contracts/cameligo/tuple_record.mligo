type r = { a : nat ; b : int  ; c : string }

let r = ( { a = 1n ; b = 1 ; c = "H" }
        , { a = 2n ; b = 2 ; c = "E" }
        , { a = 3n ; b = 3 ; c = "L" } 
        )
let ( { a = a1 ; b = b1 ; c = c1 }
    , { a = a2 ; b = b2 ; c = c2 }
    , { a = a3 ; b = b3 ; c = c3 }
    ) = r
let ( { a = a4 ; b = b4 ; c = c4 }
    , { a = a5 ; b = b5 ; c = c5 }
    , { a = a6 ; b = b6 ; c = c6 }
    ) 
  = ( { a = 4n ; b = 4 ; c = "L" }
    , { a = 5n ; b = 5 ; c = "L" }
    , { a = 6n ; b = 6 ; c = "O" } 
    )

type storage = nat * int * string

[@entry]
let main (_ : unit) (_ : storage) : operation list * storage
  = [], ( a1 + a2 + a3 + a4 + a5 + a6
        , b1 + b2 + b3 + b4 + b5 + b6
        , c1 ^ c2 ^ c3 ^ c4 ^ c5 ^ c6
        )