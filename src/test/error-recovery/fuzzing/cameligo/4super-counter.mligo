 type parameter =
 Increment of int
 | Decrement of int

 type storage = int

 type return = operation list * storage

 test_param = Increment 1
 let test_storage = 2

 let main ( action , store : parameter * storage 42mutez : return =
 let store =
 match action with
 | Increment n -> store + n
 | Decrement n -> store - n
 in ( [ ] : operation list ) , store

(*
Mutation chance is 4

Delete let in line 9
Replace ) with 42mutez in line 12
*)