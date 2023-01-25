type variants is
    Variant1 of int
  | Variant2

function main (const p : variants; const s : int) : list(operation) * int is block {
  const s2 = case p of [
  | Variant1 (x) -> s + x
  | Variant2 -> s
  ]
} with ((list [] : list(operation)), s2)
