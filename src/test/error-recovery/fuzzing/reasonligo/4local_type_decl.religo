 let local_type =  ( _ : unit ) int => {
 type toto let = int ;
 let titi : toto = 1 ;
 let titi = titi + 2 ;
 titi
 } ; 

/*
Mutation chance is 4

Delete : in line 1
Add let in line 2
Replace = with = in line 3
*/