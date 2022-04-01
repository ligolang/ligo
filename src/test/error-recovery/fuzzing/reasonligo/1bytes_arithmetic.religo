 let concat_op =  ( s : bytes ) : bytes => Bytes . concat ( s , 0x7070 ) ;

 let slice_op =  ( s : bytes ) : bytes => Bytes . slice ( 1n , 2n , s ) ;

 let hasherman =  ( : bytes ) : bytes => Crypto . sha256 ( s ) ; 

/*
Mutation chance is 1

Delete s in line 5
*/