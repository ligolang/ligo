type r is record[ a : nat ; b : int  ; c : string ]

const r = ( record[ a = 1n ; b = 1 ; c = "H" ]
          , record[ a = 2n ; b = 2 ; c = "E" ]
          , record[ a = 3n ; b = 3 ; c = "L" ] 
          )
const ( record[ a = a1 ; b = b1 ; c = c1 ]
      , record[ a = a2 ; b = b2 ; c = a2 ]
      , record[ a = a3 ; b = b3 ; c = c3 ]
      ) = r