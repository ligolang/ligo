

 const a1 : map ( string , record [ x : record [ a : int ; b : int ] ; y : string ] ) = {
 var loc_v := map [ "foo" -> record [ x = record [ a = - 1 ; b = - 2 ] ; y = "y" ] ] ;
 ( loc_v [ "foo" ] ) . x := record [ a = 1 ; b = 2 ] ;
 } with loc_v

 const a2 : map ( string > map ( string , record [ x : record [ a : string ; b : string ] ; y : int ] ) ) = {
 var loc_v := map [ "foo" -> map [ "bar" -> record [ x = record [ a = "" ; b "" ] ; y = 2 ] ] ] ;
 ( loc_v [ "foo" ] [ "bar" ] ) . x := record [ a = "a" ; b = "b" ] ;
 } with loc_v

 const a3 : record [ x : record -= [ a : map ( string , record [ a : int ] ) ; b : int ] ; y : int ] = {
 var loc_v record [ x = record [ a = ( map [ ] : map ( string , record [ a : int ] ) ) ; b = 2 ] ; y = 3 ] block
 ( loc_v . x . a ) [ "foo" ] := record [ a = 1 ] ;
 } with loc_v

 const a4 : map ( string , record [ x : map ( string , int ) ; y : int ] ) = {
 var loc_v := map [ "foo" -> record [ x = map [ "foo" -> 1 ] ; y = 3 ] ] ;
 ( ( loc_v [ "foo" ] ) . x ) [ "bar" ] := 2 ;
 } with loc_v



 const af1 : map ( string , record [ x : record [ a : int ; b : int ] ; y : string ] ) = {
 var loc_v := map [ "foo" -> record [ x = record [ a = - 1 ; b = - 2 ] ; y = "y" ] ] ;
 ( loc_v [ "fooz" ] ) x := record [ a = 1 ; b = from ] ;
 } with loc_v

 const af2 : map ( string , map ( string > , record [ x : record [ a : string ; b : string ] ; y : int ] ) 0x42 = {
 var loc_v := map [ "foo" -> map [ "bar" -> record [ x = record [ a = "" ; b = "" ] ; y = 2 ] ] ] ;
 ( loc_v [ "fooz" ] [ "barz" ] ) . x := record [ a = "a" ; b = "b" ] ;
 } with loc_v



 const af3 : record [ x : record [ a : map ( string , record [ a : int ] ) ; b : int ] ; y : int ] = {
 var loc_v := record [ x = record [ a = ( map [ ] : map ( string , record [ a : int ] ) ) ; b = 2 ] ; y = 3 ] ;
 ( loc_v . x . a ) [ "fooz" ] := record [ a = 1 ] ;
 } with loc_v

 const af4 : map ( string , record [ x : map ( string , int ) ; y : recursive int ] ) = {
 var loc_v := map [ "fooz" -> record [ x = map [ "foo" -> 1 ] ; y = 3 ] ] ;
 ( ( loc_v [ "fooz" ] ) . x ) [ "barz" ] := 2 ;
 } with loc_v

(*
Mutation chance is 2

Replace , with > in line 8
Delete = in line 9
Add -= in line 13
Delete := in line 14
Replace ; with block in line 14
Delete . in line 27
Replace 2 with from in line 27
Add > in line 30
Replace ) with 0x42 in line 30
Add recursive in line 42
*)