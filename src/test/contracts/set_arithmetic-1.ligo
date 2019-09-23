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
