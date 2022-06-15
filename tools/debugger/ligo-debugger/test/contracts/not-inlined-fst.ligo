function fst (const p : int * int) : int is p.0

function main (const _ : unit; const s : int) : list(operation) * int is block {
  var pair1 := (1, s);
  var pair2 := (3, s);
} with ((list [] : list(operation)), s + fst(pair1) + fst(pair2))
