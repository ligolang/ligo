#import "import-include/index.mligo" "I"
#include "import-include/index.mligo"

let x = hello ^ "World"

let main (_,_ : unit * string) : operation list * string = 
    [], I.hello ^ x