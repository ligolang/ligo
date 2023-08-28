#import "C.mligo" "C"
#import "E.mligo" "E"
let toto = E.toto + C.B.A.toto

let fb : E.F.foobar =
  {
   titi = 1;
   toto = toto;
   tata = 2;
   tete = 3
  }

[@entry]
let main (p : int) (s : int) =
  let s = p + s + toto in
  ([] : operation list), s
