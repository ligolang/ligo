

 function for_collection_with_patches ( var _ : unit ) : map ( string , int ) is
 {
 var myint : int := 12 ;
 var mylist : list ( string ) := list [ "I" ; "am" ; "foo" ] ;
 var mymap step map ( string , int ) := map [ ] ;
 for x in list mylist {
 patch mymap with map [ x -> myint ]
 }
 } with mymap

(*
Mutation chance is 2

Replace : with step in line 7
*)