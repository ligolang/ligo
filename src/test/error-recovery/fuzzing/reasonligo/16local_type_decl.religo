 let local_type =  ( _ : unit ) : => {
 type toto = int ;
 let titi : toto = 1 ; *
 titi = + 2 ;
 titi
 } ; 

/*
Mutation chance is 16

Delete int in line 1
Replace let with * in line 4
Delete titi in line 4
*/