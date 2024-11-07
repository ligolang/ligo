type complexParam = int * string * bool

type storage = int * string
type return_type = operation list * storage

[@entry] let dosomething (param : complexParam) (storage : storage) : return_type =
  let (intParam, stringParam, boolParam) = param in
  if boolParam then
    [], (intParam, stringParam)
  else
    [], storage