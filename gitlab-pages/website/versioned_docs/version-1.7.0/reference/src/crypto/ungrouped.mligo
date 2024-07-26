let hasherman_blake (s: bytes) : bytes = Crypto.blake2b s
let hasherman (s : bytes) : bytes = Crypto.sha256 s
let hasherman512 (s: bytes) : bytes = Crypto.sha512 s
let hasherman3 (s: bytes) : bytes = Crypto.sha3 s
let hasherman_keccak (s: bytes) : bytes = Crypto.keccak s
let check_hash_key (kh1, k2: key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2 in
  if kh1 = kh2 then (true, kh2) else (false, kh2)
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg