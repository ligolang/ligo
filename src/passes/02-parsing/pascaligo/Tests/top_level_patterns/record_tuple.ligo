type r is 
  record
  [ a : nat    * int * string
  ; b : int    * nat * string 
  ; c : string * int * nat
  ]

const r = 
  record
  [ a = (1n     , 1 , "H") 
  ; b = (2      , 2n, "E") 
  ; c = ("Hello", 3 , 3n ) 
  ]
const 
  record
  [ a = (a1, a2, a3)
  ; b = (b1, b2, b3)
  ; c = (c1, c2, c3)
  ] = r

const 
  record
  [ a = (a4, a5, a6)
  ; b = (b4, b5, b6)
  ; c = (c4, c5, c6)
  ] 
  = 
  record
  [ a = (4n     , 4 , "H") 
  ; b = (5      , 5n, "E") 
  ; c = ("Hello", 6 , 6n ) 
  ]

type storage is nat * int * string

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is (nil, 
        ( a1 + b2 + c3 + a4 + b5 + c6
        , a2 + b1 + c2 + a5 + b4 + c5
        , c1 ^ b3 ^ a3 ^ a6 ^ b6 ^ c4
        ))