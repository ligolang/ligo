#import "imported2.ligo" "IMP"

let add (a, b : int * int) : int = a + b

let zero : int = 0

let one : int = 1

let rec sum (acc, lst : int * int list) : int =
  match lst with
  | [] -> acc
  | x :: l -> sum(acc + x, l)

let rec fac (acc, n : int * int) : int =
  if n = 0
    then acc
    else fac(acc * n, n - 1)

let what (a, b, c : int * int * int) : int = IMP.strange(a, b, c) + IMP.strange(c, b, a)
