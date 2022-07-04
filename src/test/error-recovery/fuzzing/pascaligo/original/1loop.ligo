// Test while loops in PascaLIGO

// recursive to test a bugfix
recursive function counter (var n : nat) : nat is
  {
    var i : nat := 0n;
    while i < n {
      i := i + 1n
    }
  } with i

function while_sum (var n : nat) : nat is
  {
    var i : nat := 0n;
    var r : nat := 0n;
    while i < n {
      i := i + 1n;
      r := r + i
    };
    var _ := i;
  } with r

function for_sum (var n : nat) : int is
  {
    var acc : int := 0;
    for i := 1 to int (n)
      {
        acc := acc + i
      }
  } with acc

function for_sum_step (var n : nat) : int is
  {
    var acc : int := 0;
    for i := 1 to int (2n*n) step 2
      {
        acc := acc + i
      }
  } with acc

function for_collection_list (var _ : unit) : (int * string) is
  {
    var acc : int := 0;
    var st : string := "to";
    var mylist : list (int) := list [1; 1; 1];
    for x in list mylist
      {
        acc := acc + x;
        st := st ^ "to"
      }
  } with (acc, st)

function for_collection_set (var _ : unit) : int * string is
  {
    var acc : int := 0;
    var st : string := "to";
    var myset : set (int) := set [1; 2; 3];
    for x in set myset {
      acc := acc + x;
      st := st ^ "to"
    }
  } with (acc, st)

function for_collection_if_and_local_var (var _ : unit) : int is
  {
    var acc : int := 0;
    const theone : int = 1;
    const thetwo : int = 2;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset {
      if x = theone then acc := acc + x
      else if x = thetwo then acc := acc + thetwo
      else acc := acc + 10
    }
  } with acc

function for_collection_rhs_capture (var _ : unit) : int is
  {
    var acc : int := 0;
    const mybigint : int = 1000;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset {
      if x = 1 then acc := acc + mybigint
      else acc := acc + 10
    }
  } with acc

function for_collection_proc_call (var _ : unit) : int is
  {
    var acc : int := 0;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset {
      if x = 1 then
        acc := acc + for_collection_rhs_capture (unit)
      else acc := acc + 10
    }
  } with acc

function for_collection_comp_with_acc (var _ : unit) : int is
  {
    var myint : int := 0;
    var mylist : list (int) := list [1; 10; 15];
    for x in list mylist {
      if x >= myint then myint := myint + 10
    }
  } with myint

function for_collection_with_patches (var _ : unit) : map (string,int) is
  {
    var myint : int := 12;
    var mylist : list (string) := list ["I"; "am"; "foo"];
    var mymap : map (string,int) := map [];
    for x in list mylist {
      patch mymap with map [x -> myint]
    }
  } with mymap

function for_collection_empty (var _ : unit) : int is
  {
    var acc : int := 0;
    var myset : set(int) := set [1; 2; 3];
    for _x in set myset {
      skip
    }
  } with acc

function for_collection_map_kv (var _ : unit) : int * string is
  {
    var acc : int := 0;
    var st : string := "";
    var mymap : map (string, int) := map ["1" -> 1; "2" -> 2; "3" -> 3];
    for k -> v in map mymap {
      acc := acc + v;
      st := st ^ k;
    }
  } with (acc, st)

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

function nested_for_collection_local_var (var _ : unit) : int*string is
  {
    var myint : int := 0;
    var myst : string := "";
    var mylist : list (int) := list [1; 2; 3];
    for i in list mylist {
      var myst_loc : string := "";
      myint := myint + i;
      var myset : set (string) := set ["1"; "2"; "3"];
      for st in set myset {
        myint := myint + i;
        myst_loc := myst_loc ^ st;
      };
      myst := myst_loc ^ myst
    }
  } with (myint, myst)

function dummy (const n : nat) : nat is {
  while False { skip }
} with n

function inner_capture_in_conditional_block (var _ : unit) : bool * int is
  {
    var count : int := 1;
    var ret : bool := False;
    var mylist : list (int) := list [1; 2; 3];
    for _it1 in list mylist {
      for it2 in list mylist {
        if count = it2 then ret := not (ret) else skip
      };
      count := count + 1
    }
  } with (ret, count)
