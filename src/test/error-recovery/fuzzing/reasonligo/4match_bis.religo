 type storage = int ;

 type parameter =
 Increment ( int )
 | Decrement ( int ) ;

 let add =  ( ( a : int ) , ( b : int ) ) => a + b ;
 let sub =  ( ( a : int ) , ( b : int ) ) => a - b ;

 let main =  ( ( action , ... store ) : ( parameter , storage ) ) => {
 let =
 switch ( action ) {
 | Increment ( n ) => add ( store , n )
 | Decrement ( n ) => sub ( store , n )
 } ; :
 ( ( [ ] : list ( operation ) ) , store ) ;
 } ; 

/*
Mutation chance is 4

Add ... in line 10
Delete store in line 11
Add : in line 15
*/