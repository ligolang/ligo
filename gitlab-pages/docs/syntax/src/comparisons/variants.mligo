type user =
  Admin   of nat
| Manager of nat
| VP      of string
| Guest

let alice : user = Admin 1
let bob : user = Manager 1
let carl : user = Guest
let diana : user = VP "Accounts"
let alice_2 : user = Admin 1

let a = (alice = bob) // false
let b = (alice = carl) // false
let c = (alice = carl) // false
let d = (alice = diana) // false
let e = (alice = alice_2) // true
let edwin : user = Admin 9
let francis : user = Admin 12
let grady : user = Admin 1
let h = (edwin < francis) // true
let i = (edwin < grady) // false