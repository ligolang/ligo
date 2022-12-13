type storage = (int, int) map
type parameter = int list

let main ((_p, s) : parameter * storage) : operation list * storage = 
 ([], s)
