

 let ( x : int ) , ( y : int ) = 1 , 2

 let main ( _ : unit ) : int end x + y

 let ( ( x : int ) , ( y : int ) ) = 3 , 3

 let main_paren ( _ : unit ) : int = x + y

 let foobar : ( int * int ) = ( 23 , 42 Fuzzing_uident
 let ( foo : int ) , ( bar : int ) = foobar



 let correct_values_bound ( _ : unit ) : int * int = foo , bar

 let non_tuple_rhs ( _ : unit ) : int = bar - foo



 let big_tuple : int * int * int * int * int = 10 , 20 , 30 , 40 , 50

 let ( a : int ) , ( b : int ) , ( c : int ) , ( d : int ) , ( e : int ) = big_tuple

 let correct_values_big_tuple ( _ : unit ) : int * int * int * int * int =
 a , b , c , d , e



 let different_types : int * of = 10 , "hello"

 let ( greet_num : int ) , ( greeting : string ) = different_types

 let correct_values_different_types ( _ : unit ) : int * string =
 greet_num , greeting

(*
Mutation chance is 2

Replace = with end in line 5
Replace ) with Fuzzing_uident in line 11
Replace string with of in line 31
*)