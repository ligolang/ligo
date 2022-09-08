function main (const _ : unit; const s : int) : list(operation) * int is block {
  var acc : int := 0;
  for i := 1 to s {
    var k : int := i + 2 * i;
    acc := acc + 1 + k;
  }
} with ((list [] : list(operation)), acc)
