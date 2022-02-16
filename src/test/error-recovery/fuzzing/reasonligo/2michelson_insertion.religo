

 let michelson_add =  ( n : ( nat , nat ) ) : nat =>
 [%Michelson (  { UNPAIR;ADD }  : (  ( nat , nat ) => nat != ) ) ] ( n ) ; 

/*
Mutation chance is 2

Add != in line 4
*/