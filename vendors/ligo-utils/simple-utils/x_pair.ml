let compare cmpa cmpb (a1,b1) (a2,b2) = match cmpa a1 a2 with 0 -> cmpb b1 b2 | c -> c
let map f (a,b) = (f a, f b)