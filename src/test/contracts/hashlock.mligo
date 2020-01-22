type storage = {
  hashed: bytes;
  unused: bool;
}

type parameter = {
  hashable: bytes;
  message: unit -> operation list;
}

let main ((p,s): parameter * storage) : operation list * storage =
  (* We have to use a hash salted with the solvers address, otherwise a baker
     could steal *)
  let salted: bytes = Bytes.concat p.hashable (Bytes.pack sender) in
  if ((Crypto.sha256 p.hashable) = s.hashed) && s.unused
  then
    let s: storage = {hashed = s.hashed; unused = false} in
    ((p.message ()), s)
  else (([]: operation list), s)
