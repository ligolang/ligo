type variants is
    Variant1 of int
  | Variant2
  | Variant3 of option(int)

function main (const p : variants; const s : int) : list(operation) * int is block {
  const s2 = case p of [
  | Variant1 (x) -> s + x
  | Variant2 -> 0
  | Variant3 (p2) -> case p2 of [
    | Some (x) -> s + 3 * x
    | None -> s
    ]
  ]
} with ((list [] : list(operation)), s2)
