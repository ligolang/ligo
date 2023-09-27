module C = struct
  type registry = (nat, key) big_map
  type storage = {registry : registry; next_id : nat}

  let create (pub_key, s : key * storage) : storage =
    {s with
      next_id = s.next_id + 1n;
      registry = Big_map.add s.next_id pub_key s.registry}

  [@entry]
  let main (p : key) (store : storage) : operation list * storage =
    [], create (p, store)
end

let test =
  let (_, pub_key, _) = Test.get_bootstrap_account 1n in
  let orig = Test.originate (contract_of C) {registry = Big_map.empty; next_id = 1n} 0mutez in
  let () = Test.log pub_key in
  Test.transfer orig.addr (Main pub_key) 0mutez
