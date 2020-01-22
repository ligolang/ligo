type storage = {
  hashed: bytes;
  unused: bool;
}

type parameter = {
  hashable: bytes;
  message: unit -> operation list;
}

let main ((p,s): parameter * storage) : operation list * storage =
  if ((Crypto.sha256 p.hashable) = s.hashed) && s.unused
  then
    let s: storage = {hashed = s.hashed; unused = false} in
    ((p.message ()), s)
  else (([]: operation list), s)
