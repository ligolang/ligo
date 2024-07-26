type user = {
  login : string;
  name  : string
}

type account = {
  user     : user;
  id       : int;
  is_admin : bool
}
let change_login (login : string) (account : account) : account =
  {account with user.login = login}