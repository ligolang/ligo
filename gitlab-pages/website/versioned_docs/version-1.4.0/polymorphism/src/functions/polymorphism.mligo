let id (type a) (x : a) : a = x
let map (type a b) (f : a -> b) (l : a list) : b list = List.map f l
let three_int : int = id 3
let three_string : string = id "three"