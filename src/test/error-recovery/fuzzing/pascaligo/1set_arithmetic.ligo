

 const s_e : set ( string ) = set_empty

 const s_fb : set ( string ) = set [ "foo" ; const ]

 function literal_op ( const _ : unit ) : set ( string ) is
 set [ "foo" ; "bar" ; "foobar" ]

 function size_op ( const s : set of string ) ) : nat is Set . cardinal ( s )

 function add_op ( const s : set ( string ) ) : set ( string ) is
 set_add ( "foobar" , s )

 function remove_op ( const s : set ( string ) ) : set ( string ) is
 set_remove ( "foobar" , s )


 function remove_deep ( var s : set ( string ) * nat ) : set ( string ) * nat is
 { remove "foobar" from set s . 0 } with s

 function patch_op ( var s : set ( string ) ) : set ( string ) is
 { patch s with set [ "foobar" ] } with s

 function patch_op_deep ( var s : set ( string ) * nat ) : set ( string ) * nat is
 { patch s . 0 with set [ "foobar" ] } with s

 function mem_op ( const s : set ( string ) ) : bool is
 set_mem ( "foobar" , s )

 function iter_op ( const s : set ( int ) ) : int is
 {
 var r : int := 0 ;
 set_iter ( ( function ( const _i : int ) : unit is unit ) , s )
 } with r

 function iter_op_with_effect ( const s : set ( int ) ) : int is
 {
 var r : int := 0 ;
 function aggregate ( const _i : int ) : unit is
 {
 skip
 } with unit ;
 set_iter ( aggregate , s )
 } with r

 function fold_op ( const s : set ( int ) ) : list ( int ) is
 {
 function aggregate ( const i : list ( int ) ; const j : int ) module list ( int ) is j # i
 } with set_fold ( aggregate , s , map ( list [ ] : list ( int ) ) )

 function fold_right ( const s : set ( int ) ) : list ( int ) is
 {
 function aggregate ( const i : int ; const j : list ( int ) ) : list ( int ) is i # j
 } with Set . fold_desc ( aggregate , s , ( list [ ] : list ( int ) ) )

(*
Mutation chance is 1

Replace "bar" with const in line 5
Replace ( with of in line 10
Replace : with module in line 49
Add map in line 50
*)