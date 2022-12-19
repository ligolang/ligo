type big_tuple = int * int * int * int * int * int * int * int * int * int * int * int

let br : big_tuple = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

let (x0, _x1, _x2, _x3, _x4, _x5, _x61, _x7, _x8, _x9, _x45, _x111) = br

let f (x : big_tuple) =
  match x with
  | (x0, _x1, _x2, _x3, _x4, _x5, _x61, _x7, _x8, _x9, _x45, _x111) -> x0

let test = Test.assert (x0 = 0 && f br = 0)
