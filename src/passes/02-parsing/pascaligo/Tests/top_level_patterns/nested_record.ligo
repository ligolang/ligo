type t is record[ c : nat ; d : int ; e : string ]
type r is record[ a : t   ; b : t   ; c : t      ]

const r =
  record
  [ a = record[ c = 1n ; d = 1 ; e = "H" ] 
  ; b = record[ c = 2n ; d = 2 ; e = "E" ] 
  ; c = record[ c = 3n ; d = 3 ; e = "L" ]
  ]
const 
  record
  [ a = record[ c = c1 ; d = d1 ; e = e1 ]
  ; b = record[ c = c2 ; d = d2 ; e = e2 ]
  ; c = record[ c = c3 ; d = d3 ; e = e3 ] 
  ] = r
const 
  record
  [ a = record[ c = c4 ; d = d4 ; e = e4 ]
  ; b = record[ c = c5 ; d = d5 ; e = e5 ]
  ; c = record[ c = c6 ; d = d6 ; e = e6 ] 
  ]
  = 
  record
  [ a = record[ c = 4n ; d = 4 ; e = "L" ] 
  ; b = record[ c = 5n ; d = 5 ; e = "O" ] 
  ; c = record[ c = 6n ; d = 6 ; e = "O" ]
  ]

type storage is nat * int * string

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is 
    (nil, 
    ( c1 + c2 + c3 + c4 + c5 + c6
    , d1 + d2 + d3 + d4 + d5 + d6
    , e1 ^ e2 ^ e3 ^ e4 ^ e5 ^ e6 ))