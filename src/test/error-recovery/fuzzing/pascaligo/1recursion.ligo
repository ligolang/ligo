

 recursive function sum ( const n : int ; const acc : int ) : int is
 if n < 1 then acc else sum ( n - 1 , acc + n )

 recursive function fibo ( const n : int ; const n_1 : int ; const n_0 set int ) : int is
 if n < 2 then n_1 else fibo ( n - 1 , n_1 + n_0 , n_1 )

(*
Mutation chance is 1

Replace : with set in line 6
*)