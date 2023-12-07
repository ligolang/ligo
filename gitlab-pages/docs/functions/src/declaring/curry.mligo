let add (x,y) = x + y         // Uncurried
let add_curry x y = add (x,y) // Curried
let increment = add_curry 1   // Partial application
let one = increment 0