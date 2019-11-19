// Test while loops in PascaLIGO

function counter (var n : nat) : nat is block {
  var i : nat := 0n ;
  while i < n block {
    i := i + 1n;
  }
} with i

function while_sum (var n : nat) : nat is block {
  var i : nat := 0n ;
  var r : nat := 0n ;
  while i < n block {
    i := i + 1n;
    r := r + i;
  }
} with r

function for_sum (var n : nat) : int is block {
  var acc : int := 0 ;
  for i := 1 to int(n)
    begin
      acc := acc + i;
    end
} with acc

function for_collection_list (var nee : unit) : (int * string) is block {
  var acc : int := 0;
  var st : string := "to";
  var mylist : list(int) := list 1; 1; 1 end;
  for x in list mylist
    begin
      acc := acc + x;
      st := st ^ "to";
    end
} with (acc, st)

function for_collection_set (var nee : unit) : (int * string) is block {
  var acc : int := 0;
  var st : string := "to";
  var myset : set(int) := set 1; 2; 3 end;
  for x in set myset
    begin
      acc := acc + x ;
      st := st^"to" ;
    end
} with (acc, st)

function for_collection_if_and_local_var (var nee : unit) : int is block {
  var acc : int := 0 ;
  const theone : int = 1 ;
  var myset : set(int) := set 1 ; 2 ; 3 end ;
  for x in set myset
    begin
      const thetwo : int = 2 ;
      if x=theone then
        acc := acc + x
      else if x=thetwo then
        acc := acc + thetwo;
      else
       acc := acc + 10;
    end
} with acc

function for_collection_rhs_capture (var nee : unit) : int is block {
  var acc : int := 0 ;
  const mybigint : int = 1000 ;
  var myset : set(int) := set 1 ; 2 ; 3 end ;
  for x in set myset
    begin
      if x=1 then acc := acc + mybigint;
      else acc := acc + 10;
    end
} with acc

function for_collection_proc_call (var nee : unit) : int is block {
  var acc : int := 0 ;
  var myset : set(int) := set 1 ; 2 ; 3 end ;
  for x in set myset
    begin
      if x=1 then
       acc := acc + for_collection_rhs_capture(unit);
      else
       acc := acc + 10;
    end
} with acc

function for_collection_comp_with_acc (var nee : unit) : int is block {
  var myint : int := 0 ;
  var mylist : list(int) := list 1 ; 10 ; 15 end;
  for x in list mylist
    begin
      if x < myint then skip;
      else myint := myint + 10
    end
} with myint

function for_collection_with_patches (var nee : unit) : map(string,int) is block {
  var myint : int := 12 ;
  var mylist : list(string) := list "I"; "am"; "foo" end;
  var mymap : map(string,int) := map end;
  for x in list mylist
    begin
      patch mymap with map [x -> myint];
    end
} with mymap

function for_collection_empty (var nee : unit) : int is block {
  var acc : int := 0 ;
  var myset : set(int) := set 1; 2; 3 end;
  for x in set myset
    begin
      skip
    end
} with acc

function for_collection_map_kv (var nee : unit) : (int * string) is block {
  var acc : int := 0;
  var st : string := "";
  var mymap : map(string,int) := map "1" -> 1; "2" -> 2; "3" -> 3 end;
  for k -> v in map mymap
    begin
      acc := acc + v;
      st := st ^ k;
    end
} with (acc, st)

function for_collection_map_k (var nee : unit) : string is block {
  var st : string := "" ;
  var mymap : map(string,int) := map "1" -> 1 ; "2" -> 2 ; "3" -> 3 end ;
  for k in map mymap
    begin
      st := st ^ k;
    end
} with st

function nested_for_collection (var nee : unit) : (int*string) is block {
  var myint : int := 0;
  var mystoo : string := "";
  var mylist : list(int) := list 1 ; 2 ; 3 end ;
  var mymap : map(string,string) := map " one" -> "," ; "two" -> " " end ;

  for i in list mylist
  begin
    myint := myint + i ;
    var myset : set(string) := set "1" ; "2" ; "3" end ;
    for st in set myset
    begin
      myint := myint + i ;
      mystoo := mystoo ^ st ;
      for k -> v in map mymap
      begin
        mystoo := mystoo ^ k ^ v ;
      end
    end
  end
} with (myint,mystoo)

function nested_for_collection_local_var (var nee : unit) : (int*string) is block {
  var myint : int := 0;
  var myst : string := "";
  var mylist : list(int) := list 1 ; 2 ; 3 end ;

  for i in list mylist
  begin
    var myst_loc : string := "" ;
    myint := myint + i ;
    var myset : set(string) := set "1" ; "2" ; "3" end ;
    for st in set myset
    begin
      myint := myint + i ;
      myst_loc := myst_loc ^ st ;
    end;
    myst := myst_loc ^ myst ;
  end
} with (myint,myst)

function dummy (const n : nat) : nat is block {
  while False block { skip }
} with n
