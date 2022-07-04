

 function main ( i : int ) : int is
 { from
 _j : int := 0 ;
 var k : int := 1 ;
 _j := k + i ;
 k := i + _j
 } with k + _j

(*
Mutation chance is 2

Delete const in line 3
Replace var with from in line 5
*)