module Test = Test.Next

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
  let {addr=_; pk=pub_key; sk=_} = Test.Account.info 1n in
  let orig = Test.Originate.contract (contract_of C)
               {registry = Big_map.empty; next_id = 1n} 0mutez in
  let () = Test.IO.log pub_key in
  Test.Typed_address.transfer orig.taddr (Main pub_key) 0mutez
