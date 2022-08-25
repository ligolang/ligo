function main (const _ : unit; const s : int) : list(operation) * int is block {
  var acc : int := 0;
  for i := 1 to s {
    acc := acc + 1;
  }
} with ((list [] : list(operation)), acc)
