

 let test =  ( k : int ) : int => {
 let _j : int = k + 5 ;
 let close : (  int => int ) =  ( i lsr : int ) => i + _j ;

 let _j : int = 20 ;
 close ( 20 ) ;
 } ; 

/*
Mutation chance is 1

Add lsr in line 5
*/