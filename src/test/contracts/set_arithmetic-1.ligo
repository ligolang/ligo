// Test set iteration in PascaLIGO

function iter_op (const s : set(int)) : int is
  begin
    var r : int := 0 ;
    function aggregate (const i : int) : unit is
      begin
        r := r + i ;
      end with unit ;
    set_iter(s , aggregate) ;
  end with r

function fold_op (const s : set(int)) : int is
  block {
    function aggregate (const i : int ; const j : int) : int is
      i + j
  } with set_fold(s , 15 , aggregate)
