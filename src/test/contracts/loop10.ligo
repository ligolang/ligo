// Test while loops in PascaLIGO

function for_collection_empty (var _ : unit) : int is
  {
    var acc : int := 0;
    var myset : set(int) := set [1; 2; 3];
    for _x in set myset {
      skip
    }
  } with acc
