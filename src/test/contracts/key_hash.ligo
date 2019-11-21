function check_hash_key (const kh1 : key_hash; const k2 : key) : bool*key_hash is block {
  var ret : bool := False ;
  var kh2 : key_hash := crypto_hash_key(k2) ;
  if kh1 = kh2 then ret := True else skip; 
} with (ret, kh2)