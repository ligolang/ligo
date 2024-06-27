type r = { x : int; y: int }

let mk_r1 x y : r = { x; y }
let mk_r2 (x: int) y : r = { x; y }
let mk_r3 x (y: int) : r = { x; y }
let mk_r4 (x: int) (y: int) : r = { x; y }
let mk_r5 x y = { x; y }
let mk_r6 (x: int) y = { x; y }
let mk_r7 x (y: int) = { x; y }
let mk_r8 (x: int) (y: int) = { x; y }
let mk_r9 = fun x y -> { x; y }
let mk_r10 x = fun y -> { x; y }
