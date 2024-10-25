let toto = I.toto
let fb = I.fb

[@entry]
let main (p : int) (s : int) =
  let s1 = p + s + fb.titi + toto in
  ([] : operation list), s1
