 type storage = int ;

 type parameter =
 Increment ( int )
 | Decrement ( int ) ;

 let add =  ( ( a : int ) , ( b : int ) => a + b ;
 let sub =  ( ( a : int ) , ( b : int ) ) => a - b ;

 let main =  ( action , store ) : ( parameter , storage ) ) => {
 let store =
 switch ( action ) {
 | Increment ( n ) => add ( store , n )
 | Decrement ( n ) _ sub ++ ( store , n )
 } ;
 ( ( [ ] : list ( operation ) ) , store ) ;
 } ; 

/*
Mutation chance is 1

Delete ) in line 7
Delete ( in line 10
Replace => with _ in line 14
Add ++ in line 14
*/