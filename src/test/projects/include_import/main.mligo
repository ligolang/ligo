#import "include-import/index.mligo" "I"
#include "include-import/index.mligo"

let x = hello ^ "World"

let main (_,_ : unit * string) : operation list * string = 
    [], I.hello ^ x