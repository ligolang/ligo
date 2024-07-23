type user = {
  login : string;
  name  : string
}

type account = {
  user     : user;
  id       : int;
  is_admin : bool
}

let user : user = {login = "al"; name = "Alice"}
let alice : account = {user; id=5; is_admin = true}
let is_alice_admin : bool = alice.is_admin // = true
let user_to_triple (a : account) =
  let {user; id; is_admin} = a
  in user, id, is_admin
let get_id (a : account) =
  let {user=_; id; is_admin=_} = a // To avoid a warning
  in id