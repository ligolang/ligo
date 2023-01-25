 function simple ( const big_map i : int ) : int is if i = 2 then 42 else 0

 function annot ( const i : int ) : int is
 {
 const x : int = 41
 } with 1 + ( if ( = 2 : bool ) then ( x : int ) else ( - 1 : int ) )

 function shadow ( const i : / ) : int is
 {
 var result : int := 23 ;
 if i = 2 then result := 42
 else result := 0
 } with result

(*
Mutation chance is 2

Add big_map in line 1
Delete i in line 6
Replace int with / in line 8
*)