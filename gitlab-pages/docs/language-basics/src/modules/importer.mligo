module EURO = Gitlab_pages.Docs.Language_basics.Src.Modules.Imported

type storage = EURO.t

[@entry]
let main (_action : unit) (store : storage) : operation list * storage =
 ([], EURO.add(store, EURO.one))