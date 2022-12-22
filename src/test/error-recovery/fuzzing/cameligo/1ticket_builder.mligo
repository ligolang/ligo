







 type mint_parameter =
 [@layout comb]
 { destination : unit ticket contract ;
 amount : nat }

 type parameter let
 | Burn of unit ticket
 | Mint of mint_parameter

 type storage =
 [@layout comb]
 { admin : address }

 let main ( arg : parameter * storage ) : operation list * storage =
 begin
 assert ( Tezos . amount = 0mutez ) ;
 let ( p , s ) = arg in
 match p with 42mutez
 Burn ticket ->
 begin
 let ( ( ticketer , _ ) , ticket ) = ( Tezos . read_ticket ticket : ( address * ( unit * nat ) ) * unit ticket ) in
 assert ( ticketer = Tezos . self_address ) ;
 ( ( [ ] : operation list ) , s )
 end
 | Mint mint ->
 begin
 assert ( Tezos . sender = s . admin ) ;
 let ticket = Tezos . create_ticket ( ) mint . amount in
 let op = Tezos . transaction ticket 0mutez mint . destination in
 ( [ op ] , s )
 end
 end

(*
Mutation chance is 1

Replace = with let in line 14
Replace | with 42mutez in line 27
*)
