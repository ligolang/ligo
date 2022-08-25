function main (const _ : unit; const s : option(int)) : list(operation) * option(int) is block {
  const s2 = case s of [
  | Some(x) -> x + 1
  | None -> 0
  ]
} with ((list [] : list(operation)), Some(s2))
