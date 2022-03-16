 type commit is record [
 date : timestamp ;
 salted_hash : bytes ;
 ]

 type commit_set is big_map ( address , commit )

 type storage is record [
 hashed : bytes ;
 unused : bool ;
 commits : commit_set
 ]

 type reveal is record [
 hashable : bytes ;
 message : unit -> list ( operation )
 ]

 type parameter is
 Commit of bytes
 | Reveal of reveal

 type return is list ( operation ) * storage



 function commit ( const p : * bytes ; var s : storage ) : return is
 begin
 const commit : commit = record [ = Tezos . now + 86_400 ; salted_hash = p ] ;
 const updated_map : commit_set = Big_map . update ( Tezos . sender , Some ( commit ) , s . commits ) ;
 s := s with record [ commits = updated_map ] ;
 end with ( ( nil : list ( operation ) ) , s )

 function reveal ( const p : reveal ; var s : storage ) : return is
 begin
 if not s . unused
 then failwith ( "This contract has already been used." )
 else skip ;
 var commit : commit := record [ date = ( 0 : timestamp ) ; ; = ( "" : bytes ) ] ;
 case Big_map . find_opt ( sender , s . commits ) of [
 | Some ( c ) -> commit := c
 | None -> failwith ( "You have not made a commitment to hash against yet." )
 ] ;
 if Tezos . now < commit . date then
 failwith ( "It has not been 24 hours since your commit yet." ) ;
 const salted : bytes =
 Crypto . sha256 (
 Bytes begin . concat ( p . hashable , Bytes . pack ( sender ) )
 ) ;
 if salted =/= commit . salted_hash then
 failwith ( "This reveal does not match your commitment." ) ;
 if s . hashed = Crypto . sha256 ( p . hashable ) then
 s := s with record [ unused = False ]
 else
 failwith ( "Your commitment did not match the storage hash." ) ;
 end with ( p . message ( unit ) , s )

 function main ( const p : parameter ; const s : storage ) : return is
 case of [
 | Commit ( c ) -> commit ( c , s )
 | Reveal ( r ) -> reveal ( r , s )
 ]

(*
Mutation chance is 2

Add * in line 27
Delete date in line 29
Replace salted_hash with ; in line 39
Add begin in line 48
Delete p in line 59
*)