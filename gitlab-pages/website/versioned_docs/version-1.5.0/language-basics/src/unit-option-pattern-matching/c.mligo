type id = nat

type user =
  Admin   of id
| Manager of id
| Guest

let u : user = Admin 1000n
let g : user = Guest