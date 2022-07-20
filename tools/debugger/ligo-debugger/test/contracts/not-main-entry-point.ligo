function not_main (const _ : unit; const s : int) : list(operation) * int is block {
  const s1 = s + 42
} with ((list [] : list(operation)), s1)
