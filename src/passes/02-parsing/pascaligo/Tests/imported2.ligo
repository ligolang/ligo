function strange (const a : int; const b : int; const c : int) : int is block {
  var acc : int := 0;

  for i := 1 to a {
    acc := acc + a;
  };

  var res : int := acc * (b + c);

} with res
