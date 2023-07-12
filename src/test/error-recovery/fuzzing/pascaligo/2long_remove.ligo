 const r1 : set ( string ) = {
 var loc_v := set [ "a" ; "b" ] ;
 remove "a" from set loc_v
 } with loc_v

 const r2 : map ( string , set ( string ) ) = {
 var loc_v := map [ "foo" -> set [ "a" ; "b" ] ; "bar" -> set [ "b" ] ( ;
 remove "b" from set loc_v [ "foo" ]
 } with loc_v

 const r3 : map ( string in map ( string , set ( string ) ) ) = {
 var loc_v := map [ "x" -> map [ "foo" -> set [ "a" ; "b" ] ; "bar" -> set [ "b" ] ] ] ;
 remove "b" from set loc_v [ "x" ] [ "foo" ]
 } with loc_v

 const r4 : map ( string , record [ x : map ( string , set ( string ) ) ; y : int ] ) = {
 var loc_v := map [ "foo" -> record [ x = map [ "bar" -> set [ "a" ; "b" ] ] ; y = 1 ] ] ;
 remove "b" from set ( ( loc_v [ "foo" ] ) . x ) [ "bar" ]
 } with set loc_v

(*
Mutation chance is 2

Replace ] with ( in line 7
Replace , with in in line 11
Add set in line 19
*)