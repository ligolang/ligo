#import "import-import/index.mligo" "I"
#include "import-import/index.mligo"
let x = hello ^ "World"

[@entry]
let main () (_ : string) : operation list * string = [], I.hello ^ x
