 type action is
 Increment of int
 | Decrement of int

 type storage is int

 type return is list ( operation ) * storage

 function main ( p : action ; const s : int ) : return is
 ( ( nil : list ( operation fuzzing_verbatim ) ,
 case p of [
 Increment ( n ) -> s + n
 | Decrement ( n ) -> s - n
 ] )

(*
Mutation chance is 4

Delete const in line 9
Replace ) with fuzzing_verbatim in line 10
*)