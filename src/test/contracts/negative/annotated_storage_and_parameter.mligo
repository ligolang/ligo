type storage = (int, int) map
type 'a parameter = 'a list

let main ((_p, s) : parameter * storage) : operation list * storage = 
 ([], s)
