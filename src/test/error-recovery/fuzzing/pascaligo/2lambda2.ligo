 type storage is unit ;





 function main ( const _unit : unit ; _storage : unit ) : unit is
 {
 const toto : ( unit -> unit ) -> unit = function ( const f : ( unit -> unit ) ) : unit is f ( Unit ) ;
 } with toto ( function ( const _unit : unit ) is Unit )

(*
Mutation chance is 2

Delete const in line 7
*)