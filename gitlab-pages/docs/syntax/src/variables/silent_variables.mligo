[@entry] let reset (_param: unit) (_storage : int) : operation list * int = [], 0
type user = {
  id       : int;
  is_admin : bool;
  name     : string
}

let getUserID (user : user) : int =
  let { id; is_admin; name } = user in
  let () = ignore ([is_admin, name]) in
  id