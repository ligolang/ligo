[@entry] let reset (_param: unit) (_storage : int) : operation list * int = [], 0
let f () =
  let user = {name = "Alice"; id = 5n} in
  let { name; id } = user in
  ignore (name, id)