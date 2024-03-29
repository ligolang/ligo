#import "./gitlab-pages/docs/language-basics/src/modules/imported.mligo" "EURO"

type storage = EURO.t

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
 ([], EURO.add(store, EURO.one))