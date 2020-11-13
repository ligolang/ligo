#import "C.mligo" "C"
#import "E.mligo" "E"

let toto = E.toto

let main ((p,s) : int * int) =
	let s = p + s + toto in
	([]: operation list),s
