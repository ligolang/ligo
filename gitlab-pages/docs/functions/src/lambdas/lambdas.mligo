let sum = fun x y -> x + y      // Uncurried
let add = fun (x,y) -> x + y    // Curried
let increment = fun x -> x + 1