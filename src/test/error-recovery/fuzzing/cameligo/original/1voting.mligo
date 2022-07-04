

let main (p, _s : key * (nat*nat)) : operation list * (nat*nat) =
  let x = Tezos.voting_power (Crypto.hash_key p) in
  let y = Tezos.get_total_voting_power () in
  ([] : operation list), (x,y)