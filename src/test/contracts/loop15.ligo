// Test while loops in PascaLIGO

function nested_for_collection (var _ : unit) : int * string is
  {
    var myint : int := 0;
    var mystoo : string := "";
    var mylist : list(int) := list [1; 2; 3];
    var mymap : map (string, string) := map [" one" -> ","; "two" -> " "];
    for i in list mylist {
      myint := myint + i;
      var myset : set (string) := set ["1"; "2"; "3"];
      for st in set myset {
        myint := myint + i;
        mystoo := mystoo ^ st;
        for k -> v in map mymap {
          mystoo := mystoo ^ k ^ v
        }
      }
    }
  } with (myint, mystoo)
