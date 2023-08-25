#import "imported.mligo" "IMP"

let fst (p : int * int) : int = p.0

[@entry]
let main () (s : int) : operation list * int =
  let fact = IMP.fac(1, s) in
  let lst = [1;2;3;4] in
  let sumLst = IMP.sum(0, lst) in
  let strangeSum = IMP.add(IMP.zero, IMP.one) in
  (([] : operation list), fact + sumLst + strangeSum + IMP.what(s, s + 1, s + 2))
