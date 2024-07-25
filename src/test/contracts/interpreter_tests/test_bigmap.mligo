module Test = Test.Next

module C = struct
  type storage = (int, nat) big_map

  [@entry] let main ((k, v) : int * nat) (s : storage) : operation list * storage =
    ([] : operation list), Big_map.add k v s
end

let test =
  let init = Big_map.add 12 42n Big_map.empty in
  let _ = Test.Originate.contract (contract_of C) init 0tez in
  let init = Big_map.add 32 42n Big_map.empty in
  let orig = Test.Originate.contract (contract_of C) init 0tez in
  let ctr = Test.to_contract orig.taddr in
  let m_old = Test.Typed_address.get_storage orig.taddr in
  let () = Test.IO.log m_old in
  let () = Test.IO.log (Big_map.find_opt 21 m_old) in
  let _ = Test.Contract.transfer_exn ctr (Main (21, 42n)) 0tez in
  let _ = Test.Contract.transfer_exn ctr (Main (3, 42n)) 0tez in
  let m_new = Test.Typed_address.get_storage orig.taddr in
  let () = Test.IO.log m_old in
  let () = Test.IO.log m_new in
  let () = Test.IO.log (Big_map.find_opt 21 m_old) in
  let () = Test.IO.log (Big_map.find_opt 21 m_new) in
  ()
