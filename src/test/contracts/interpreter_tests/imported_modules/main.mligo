#import "a.mligo" "A"
#import "b.mligo" "B"

type action = unit

[@entry]
let main (() : B.b) (_storage : A.a) =
  let (a,_,_) = A.a_v "hey" in
  let ((),_,_) = B.b_v () in
  ([]:operation list), a