const r = ((1n, 1, "H"), (2n, 2, "E"), (3n, 3, "Hello"))
const ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) = r
const ((a4, a5, a6), (b4, b5, b6), (c4, c5, c6)) 
    = ((4n, 4, "L"), (5n, 5, "O"), (6n, 6, "World"))

type storage is nat * int * string

function main (const _ : unit; const _ : storage) : list(operation) * storage
  is (nil, 
        (a1 + b1 + c1 + a4 + b4 + c4, 
         a2 + b2 + c2 + a5 + b5 + c5, 
         a3 ^ b3 ^ c3 ^ a6 ^ b6 ^ c6))