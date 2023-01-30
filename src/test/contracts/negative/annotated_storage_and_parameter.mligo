type storage = (int, int) map
type 'a parameter = 'a list

let main (_p : parameter) (s : storage) : operation list * storage = 
 ([], s)
