type r is record[ a : nat ; b : int ; c : string ]

const r = record[ a = 1n ; b = 1 ; c = "Hello" ]
const record[ a ; b ; c ] = r
const record[ a = a1 ; b = b1 ; c = c1 ] 
    = record[ a = 2n ; b = 2 ; c = "World" ]

type storage is nat * int * string

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is (nil, (a + a1, b + b1, c ^ c1))