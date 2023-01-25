 type inner_storage is michelson_pair ( int , "one" , nat , "two" )
 type storage is michelson_pair ( string , "three" , inner_storage , "" )

 type return is list ( operation ) * storage

 function main ( const action if unit ; const store : storage ) : return is {
 const foo : storage = ( "foo" , ( 1 , 2n ) ) ;
 } with ( ( nil : list ( operation ) ) , ( foo : storage ) )

(*
Mutation chance is 2

Replace : with if in line 6
*)