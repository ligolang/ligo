type registry = (nat, key) big_map
type storage = {registry : registry; next_id : nat}

let create (pub_key, s : key * storage) : storage =
  {s with
    next_id = s.next_id + 1n;
    registry = Big_map.add s.next_id pub_key s.registry}

let main (p, store : key * storage) : operation list * storage =
  [], create (p, store)

let test =
  let (_, pub_key, _) = Test.get_bootstrap_account 1n in
  let (taddr, _, _) = Test.originate main {registry = (Big_map.empty : registry); next_id = 1n} 0mutez in
  let contr = Test.to_contract taddr in
  let () = Test.log pub_key in
  Test.transfer_to_contract contr pub_key 0mutez
