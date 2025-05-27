type coin = Head | Tail
let head : coin = Head
let tail : coin = Tail
type id = nat

type user =
  Admin   of id
| Manager of id
| Guest

let bob : user = Admin 1000n
let carl : user = Guest