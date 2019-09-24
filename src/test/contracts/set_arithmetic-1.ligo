// Test set iteration in PascaLIGO

function iter_op (const s : set(int)) : int is
  var r : int := 0 ;
  function aggregate (const i : int) : unit is
  begin
    r := r + i ;
  end with unit
  begin
    set_iter(s , aggregate) ;
  end with r

function fold_op (const s : set(int)) : int is
  function aggregate (const i : int ; const j : int) : int is
  block { skip } with i + j
  block { skip } with set_fold(s , 15 , aggregate)
