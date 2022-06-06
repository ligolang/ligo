type variants =
  | Variant1 of int
  | Variant2

let main (p, s : variants * int) : operation list * int =
  let s2 = match p with
    | Variant1 x -> s + x
    | Variant2 -> s
    in
  (([] : operation list), s2)
