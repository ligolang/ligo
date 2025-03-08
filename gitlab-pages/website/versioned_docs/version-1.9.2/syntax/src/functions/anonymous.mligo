let sum = fun x y -> x + y      // Uncurried function expression
let add = fun (x,y) -> x + y    // Curried function expression
let increment = fun x -> x + 1