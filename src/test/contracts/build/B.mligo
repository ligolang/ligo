#import "A.mligo" "A"

let f (((),x) : unit*int) =
    let titi = 1 in
    let x = x + A.toto + titi in
    ([] : operation list),x
