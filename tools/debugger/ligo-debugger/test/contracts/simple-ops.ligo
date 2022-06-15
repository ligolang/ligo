function main (const _ : unit; const s : int) : list(operation) * int is block {
  const s2 = s + 42;
  const s3 = s2 * s2 * 2;
} with ((list [] : list(operation)), s2)
