function main (const p : int; const s : list(int)) : list(operation) * list(int) is block {
  const s2 : list(int) = case s of [
    nil -> list [p]
  | x # l -> x # p # x # l
  ]
} with ((list [] : list(operation)), s2)
