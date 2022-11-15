#import "import-import/index.mligo" "I"
#include "import-import/index.mligo"

let x = hello ^ "World"

let main (_,_ : unit * string) : operation list * string = 
    [], I.hello ^ x