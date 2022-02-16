 let local_type ( _ : unit ) : int = fuzzing_ident
 type toto = int in
 let titi : toto = 1 in
 let titi = titi + 2 in
 titi

(*
Mutation chance is 2

Add fuzzing_ident in line 1
*)