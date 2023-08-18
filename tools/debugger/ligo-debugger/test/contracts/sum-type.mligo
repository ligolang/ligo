type parameter =
  | Variant1 of int
  | Variant2

[@entry]
let main (p, s : parameter * int) : operation list * int =
  let x = s + s in
  let y =
    match p with
    | Variant1 i -> i
    | Variant2 -> 42
  in (([] : operation list), x + y)
