type t is record[ c : nat ; d : int ; e : string ]
type r is record[ a : t   ; b : t   ; c : t      ]

const r = record 
        [ a = record[ c = 1n ; d = 1 ; e = "H" ] 
        ; b = record[ c = 2n ; d = 2 ; e = "E" ] 
        ; c = record[ c = 3n ; d = 3 ; e = "L" ]
        ]

const record
        [ a = record[ c = c1 ; d = d1 ; e = e1 ]
        ; b = record[ c = c2 ; d = d2 ; e = e2 ]
        ; c = record[ c = c3 ; d = c1 ; e = e3 ] 
        ] = r