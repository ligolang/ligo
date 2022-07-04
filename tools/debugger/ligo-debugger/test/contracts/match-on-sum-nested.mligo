type variants =
  | Variant1 of int
  | Variant2
  | Variant3 of int option

let main (p, s : variants * int) : operation list * int =
  let s2 = begin match p with
    | Variant1 x -> s + x
    | Variant2 -> 0
    | Variant3 p2 -> match p2 with
      | Some x -> s + 3 * x
      | None -> s
    end in
  (([] : operation list), s2)
