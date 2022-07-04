// Test while loops in PascaLIGO

function for_collection_list (var _ : unit) : (int * string) is
  {
    var acc : int := 0;
    var st : string := "to";
    var mylist : list (int) := list [1; 1; 1];
    for x in list mylist
      {
        acc := acc + x;
        st := st ^ "to";
      }
  } with (acc, st)
