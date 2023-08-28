#import "import-include/index.mligo" "I"
#include "import-include/index.mligo"
let x = hello ^ "World"

[@entry]
let main () (_ : string) : operation list * string = [], I.hello ^ x
