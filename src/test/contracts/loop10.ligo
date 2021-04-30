// Test while loops in PascaLIGO

function for_collection_empty (var nee : unit) : int is
  block {
    var acc : int := 0;
    var myset : set(int) := set [1; 2; 3];
    for x in set myset block {
      skip
    }
  } with acc