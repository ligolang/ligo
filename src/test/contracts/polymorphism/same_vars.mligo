let id1 (type x y) (f : x -> y) : x -> y = f
let id2 (type z) (f : z -> unit) : z -> unit = id1 f
let id3 (type y) (f : y -> unit) : y -> unit = (id1 (f : y -> unit) : y -> unit)
let id4 (type x y) (f : y -> x) : y -> x = id1 f
let id5 (type y x) (f : y -> x) : y -> x = id1 f

let s (type a b c) (f : a -> b -> c) (g : a -> b) (x : a) : c = f x (g x)
let s2 (type b c) (f : b -> c -> unit) (g : b -> c) (x : b) : unit = s f g x

let x = id4 (fun (x : int) -> abs x) (-4)
let y = id5 (fun (x : int) -> abs x) (-4)
