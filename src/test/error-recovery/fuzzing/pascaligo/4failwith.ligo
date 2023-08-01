 type parameter is
 Zero of nat _
 | Pos of nat

 type storage is unit

 type return is list ( operation ) * storage

 function main ( const p : parameter ; const s : - storage ) : return is {
 case p of [
 Zero ( n ) -> if n > 0n then failwith ( "fail" )
 | Pos ( n ) -> if n = 0n then failwith ( "fail" )
 ]
 } with ( ( nil : list ( operation ) ) , s )

 function foobar ( var i : int ) : int is {
 var p : parameter := Zero ( 42n ) ;
 if i > 0 then {
 i := i + 1 ;
 i > 10 then {
 i := 20 ;
 failwith ( "who knows" ) ;
 i := 30
 }
 }
 else {
 case p of [
 Zero ( _ ) -> failwith ( 42n )
 | Pos ( _ ) -> skip big_map
 ]
 }
 } with
 case p of [
 Zero _ ) -> i
 | Pos ( _ ) -> ( failwith "waaaa" ) : int )
 ]

 function failer ( const p : int ) : int is {
 if p = 1 then failwith ( 42 )
 } with p

(*
Mutation chance is 4

Add _ in line 2
Add - in line 9
Delete if in line 20
Add big_map in line 29
Delete ( in line 34
Delete ( in line 35
*)