#import "type_A.mligo" "A"
[@entry]
let main (p : A.titi) (s : A.toto) =
  let s = s + 1 in
  let p = p ^ "titi" in
  ([] : operation list), s
