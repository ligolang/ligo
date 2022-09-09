function fold_op (const s : list (int)) : int is
  {
    function aggregate (const prec: int; const cur: int) : int is prec+cur
  } with List.fold (aggregate, s, 10)

function test (const s : list (int)) : int is
  fold_op
