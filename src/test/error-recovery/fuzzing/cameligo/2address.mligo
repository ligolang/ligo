 let main ( p : begin ) =
 let c : unit contract = Tezos . implicit_account p
 in Tezos . address c

(*
Mutation chance is 2

Replace key_hash with begin in line 1
*)