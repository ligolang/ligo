// Test while loops in PascaLIGO

function counter (var n : nat) : nat is block {
  var i : nat := 0n ;
  while (i < n) block {
    i := i + 1n ;
  }
} with i

function while_sum (var n : nat) : nat is block {
  var i : nat := 0n ;
  var r : nat := 0n ;
  while (i < n) block {
    i := i + 1n ;
    r := r + i ;
  }
} with r

function for_sum (var n : nat) : int is block {
  var acc : int := 0 ;
  for i := 1 to int(n)
    begin 
      acc := acc + i ;
    end
} with acc 

function for_collection_list (var nee : unit) : (int * string) is block {
  var acc : int := 0 ;
  var st : string := "to" ;
  var mylist : list(int) := list 1 ; 1 ; 1 end ;
  for x : int in list mylist
  begin
    acc := acc + x ;
    st := st^"to" ;
  end
} with (acc, st)

function for_collection_set (var nee : unit) : (int * string) is block {
  var acc : int := 0 ;
  var st : string := "to" ;
  var myset : set(int) := set 1 ; 2 ; 3 end ;
  for x : int in set myset
  begin
    acc := acc + x ;
    st := st^"to" ;
  end
} with (acc, st)

// function for_collection_assignements_in_ifs (var nee : unit) : int is block {
//   var acc : int := 0 ;
//   var myset : set(int) := set 1 ; 2 ; 3 end ;
//   for x : int in set myset
//   begin
//     if (x=1) then 
//      acc := acc + x ;
//     else 
//      acc := acc + 10 ;
//   end
// } with acc

function for_collection_empty (var nee : unit) : int is block {
  var acc : int := 0 ;
  var myset : set(int) := set 1 ; 2 ; 3 end ;
  for x : int in set myset
  begin
    skip ;
  end
} with acc

// function for_collection_map (var nee : unit) : (int * string) is block {
//   var acc : int := 0 ;
//   var st : string := "" ;
//   var mymap : map(string,int) := map "one" -> 1 ; "two" -> 2 ; "three" -> 3 end ;
//   for k -> v : (string * int) in map mymap
//   begin
//     acc := acc + v ;
//     st := k^st ;
//   end
// } with (acc, st)

function dummy (const n : nat) : nat is block {
  while (False) block { skip }
} with n
