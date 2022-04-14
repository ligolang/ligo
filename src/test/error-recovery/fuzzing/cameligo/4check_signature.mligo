 let check_signature ( pk , 42mutez , msg : key * signature * bytes ) : bool =
 Crypto . check pk signed msg

















 let example : bool =
 Crypto . check
 ( "edpktz4xg6csJnJ5vcmMb2H37sWXyBDcoAp3XrBvjRaTSQ1zmZTeRQ" : key )
 ( mod "edsigtnzKd51CDomKVMFBoU8SzFZgNqRkYUaQH4DLUg8Lsimz98DFB82uiHAkdvx29DDqHxPf1noQ8noWpKMZoxTCsfprrbs4Xo" : signature )
 0x05010000000568656c6c6f

(*
Mutation chance is 4

Replace signed with 42mutez in line 1
Add mod in line 23
*)