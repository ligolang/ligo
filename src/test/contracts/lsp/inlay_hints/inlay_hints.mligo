let f1 = 42

let f2 : int = 42

let f3 (type a) = 42

let f4 x = fun y -> x + y

let f5 (x) = fun (y) -> x + y

let f6 (x, y) = x

let f7 = fun (x, y) z -> if z then x else y

let rec f8 (x : int) : int = if x = 1 then 1 else f8 (x - 1) * x

let rec f9 : int -> int = fun x -> if x = 1 then 1 else f9 (x - 1) * x

let f10 =
  for x = 0 upto 10 do
  done

// Inlay hints are omitted for recovered arguments
let f11 (x y z) = ()

let f12 =
  let f x y = x + y in
  let rec g (x : int) : int = x - 1 in
  let rec h : int -> int = fun x -> x - 1 in
  let a (x, y) = x + y in
  let b = fun (x, y) -> x + y in
  ()

type 'a t = Box of 'a

let f13 (Box x) = x

let f14 box =
  match box with
  | Box a -> a

let f15 (lst : int list) =
  for x in lst do
  done

let f16 =
  let mut res = false in
  while not res do
    res := true
  done

type 'a r = { x : 'a; y : int; z : string }

let f17 (r : int r) =
  let { x; y; z } = r in
  let { x = x1; y = y1; z = z1 } = r in
  ()

let f18 {x; y; z} = x
