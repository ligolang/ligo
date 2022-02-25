 type fuzzing_ident storage = ( int , string , nat , bool )

 type parameter = int

 let or =  ( ( p , storage ) : ( parameter , storage ) ) => {
 ( [ ] : list ( operation ) , ( 2 , "2" , 2n , false ) ) ;
 } ; 

/*
Mutation chance is 8

Add fuzzing_ident in line 1
Replace main with or in line 5
*/