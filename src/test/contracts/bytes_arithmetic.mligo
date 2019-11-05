let concat_op (s : bytes) : bytes =
   Bytes.concat s ("7070" : bytes)

let slice_op (s : bytes) : bytes =
   Bytes.slice 1p 2p s

let hasherman (s : bytes) : bytes =
   Crypto.sha256 s
