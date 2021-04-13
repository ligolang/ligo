// Test while loops in PascaLIGO

function for_collection_proc_call (var nee : unit) : int is
  block {
    var acc : int := 0;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = 1 then
        acc := acc + for_collection_rhs_capture (unit)
      else acc := acc + 10
    }
  } with acc