#import "A.mligo" "A"
let toto = 32

let titi = A.toto + 42

[@entry]
let main () (x : int) =
  let x = x + A.toto + titi in
  ([] : operation list), x
