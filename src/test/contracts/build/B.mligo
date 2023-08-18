#import "A.mligo" "A"

let toto = 32
let titi = A.toto + 42

let main (((),x) : unit*int) =
    let x = x + A.toto + titi in
    ([] : operation list),x
