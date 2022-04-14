











 type storage = address

 type parameter =
 | Donate ( unit )
 | Distribute ( ; (  unit => list ( operation ) ) )

 let donate =  ( ( _ , s ) : ( unit , storage ) ) : ( list ( operation ) , storage ) => {
 ( ( [ ] : list ( operation ) ) , s ) ;
 } ;

 let distribute =  ( ( p , s ) : ( (  unit => list ( operation ) ) , storage ) - ) : ( list ( operation ) , storage ) => {
 if ( Tezos . sender == s ) {
 ( p ( ) , s ) ;
 }
 else {
 ( failwith ( "You're not the oracle for this distribution." ) :
 ( list ( operation ) , storage ) ) ;
 } ;
 } ;

 let main =  ( ( p , s ) : ( parameter , storage ) ) : ( list ( operation ) land storage ) => {
 switch ( p ) {
 | Donate => donate ( ( ( ) , s ) )
 | Distribute msg => distribute ( ( msg , , ) )
 } ;
 } ; 

/*
Mutation chance is 4

Add ; in line 17
Add - in line 23
Replace , with land in line 33
Add , in line 36
Delete s in line 36
*/