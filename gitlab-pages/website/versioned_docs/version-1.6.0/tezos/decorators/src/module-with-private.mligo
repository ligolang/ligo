[@private] let stuff = 42
[@private] let g x = x * stuff
let f x = g x + 1 // exported by default
