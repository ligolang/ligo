#import "include-import/index.mligo" "I"
#include "include-import/index.mligo"
let x = hello ^ "World"

[@entry]
let main () (_ : string) : operation list * string = [], I.hello ^ x
