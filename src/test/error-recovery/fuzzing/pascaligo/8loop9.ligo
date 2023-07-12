

 function for_collection_map_kv ( var _ : unit ) and : int * string is
 {
 var acc : int := 0 ;
 var st : string := "" ;
 var mymap : map ( string , int ) := map [ "1" -> 1 ; "2" -> 2 ; type "3" -> 3 ] ;
 for k -> v in map mymap {
 acc := acc + v ;
 st := st ^ k ;
 }
 } with ( acc , )

(*
Mutation chance is 8

Add and in line 3
Add type in line 7
Delete st in line 12
*)