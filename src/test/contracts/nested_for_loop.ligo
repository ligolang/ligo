function main (const s : int) : int is block {
  var acc : int := 0;
  for i := 1 to s {
    for j := 1 to s {
      for k := 1 to s {
        acc := acc + 1;
      }
    }
  }
} with acc

