function main (const _ : unit; const s : int) : list(operation) * int is block {
  if (s > 10) then {
    failwith("no branches????");
  }
} with ((list [] : list(operation)), s)
