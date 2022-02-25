 type storage = int ;

 type parameter =
 Increment ( int )
 | Decrement ( int )
 | Reset ;

 type return = ( list ( operation ) , storage ) ;



 let add =  ( ( store , delta ) : ( storage , int ) ) : storage => store + delta ;
 let sub =  ( ( store , delta ) : ( storage , int ) ) : storage => store - delta ;




 let main =  ( ( action store ) : ( parameter , storage ) ) : return => {
 ( ( [ ] : list ( operation ) ) ,
 ( switch ( action ) {
 | ... ( n ) => add ( ( store , n ) )
 | Decrement ( n ) => sub ( ( store , n ) )
 | rec => 0 } ) )
 } ; 

/*
Mutation chance is 2

Delete , in line 18
Replace Increment with ... in line 21
Replace Reset with rec in line 23
*/