type user = {
  id       : nat;
  is_admin : bool;
  name     : string
}
let alice : user = {
  id       = 1n;
  is_admin = true;
  name     = "Alice"
}
let alice_admin : bool = alice.is_admin
let user_to_tuple (u : user) =
  let { id ; is_admin ; name } = u in
  (id, is_admin, name)
let get_id (u : user) =
  let { id ; is_admin = _ ; name = _ } = u in
  id