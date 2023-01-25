type r is record[ a : nat ; b : int  ; c : string ]

const r 
  = ( record[ a = 1n ; b = 1 ; c = "H" ]
    , record[ a = 2n ; b = 2 ; c = "E" ]
    , record[ a = 3n ; b = 3 ; c = "L" ] 
    )

const 
  ( record[ a = a1 ; b = b1 ; c = c1 ]
  , record[ a = a2 ; b = b2 ; c = c2 ]
  , record[ a = a3 ; b = b3 ; c = c3 ]
  ) = r

const 
  ( record[ a = a4 ; b = b4 ; c = c4 ]
  , record[ a = a5 ; b = b5 ; c = c5 ]
  , record[ a = a6 ; b = b6 ; c = c6 ]
  ) = 
  ( record[ a = 4n ; b = 4 ; c = "L" ]
  , record[ a = 5n ; b = 5 ; c = "L" ]
  , record[ a = 6n ; b = 6 ; c = "O" ] 
  )

type storage is nat * int * string

function main (const _ : unit; const _ : storage) 
  : list(operation) * storage
  is (nil, 
        ( a1 + a2 + a3 + a4 + a5 + a6
        , b1 + b2 + b3 + b4 + b5 + b6
        , c1 ^ c2 ^ c3 ^ c4 ^ c5 ^ c6
        ))