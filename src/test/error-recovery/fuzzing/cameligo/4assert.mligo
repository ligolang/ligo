 let land main ( p , s : bool * unit ) = if
 ( ) : unit = assert p
 in ( [ ] : operation list ) , s

 let with_error ( p , s : bool * unit ) =
 let _ : unit = assert_with_error p "my custom error" in
 ( [ ] : operation list ) , s

 let let ( o : unit option ) =
 assert_some o

 let some_with_error ( o : unit option ) =
 assert_some_with_error o "my custom error"

 let none ( o : unit option ) =
 assert_none o

 let none_with_error ( o : unit option ) =
 assert_none_with_error o "my custom error"

(*
Mutation chance is 4

Add land in line 1
Replace let with if in line 2
Replace some with let in line 9
*)