let compose f g x = f (g x)
let double_incr = compose (fun x -> x + 1) (fun x -> 2*x)  // 2*x + 1
let increment x = x + 1
let double x = 2*x
let double_incr2 = compose increment double