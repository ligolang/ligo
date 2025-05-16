type ('a, 'r) treef = 'a * 'r list

type 'a tree = ('a, bytes) treef

let mufold (type a) (t : (a, a tree) treef) : a tree =
  let a, s = t in
  a, List.map Bytes.pack s

let munfold (type a) (t : a tree) : (a, a tree) treef =
  let a, bs = t in
  let f bs =
    Option.value_with_error "invalid bytes" (Bytes.unpack bs : a tree option) in
  a, List.map f bs

let node (type a) (x : a) (ts : (a tree) list) : a tree =
  mufold (x, ts)

let rec fold (type a b) (f : a -> b list -> b) (t : a tree) : b =
  let a, ts = munfold t in
  f a (List.map (fold f) ts)

let t = node 42 [node 5 []; node 2 [node 1 []]; node 3 []]

let sum =
  let rec sum_folder (n : int) (l : int list) : int =
    match l with
    | [] -> n
    | x :: xs -> x + sum_folder n xs in
  fold sum_folder
