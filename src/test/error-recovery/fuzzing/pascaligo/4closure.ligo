 function toto ( const i : int ) is : int is
 block {
 function tata ( const j : int ) : int is i + j ;
 function titi ( const j : int ) : int is i + j
 } with tata ( i ) + titi ( i )

(*
Mutation chance is 4

Add is in line 1
*)