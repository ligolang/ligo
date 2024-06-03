let ( <@ ) f g x = f (g x)
let uncurry f (x, y) = f x y
