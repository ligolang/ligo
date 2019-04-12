let compose = fun f g x -> f (g x)
let (>|) = compose
