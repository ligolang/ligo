
module C = struct
  type storage = (int, nat) big_map

  [@entry] let main ((k, v) : int * nat) (s : storage) : operation list * storage =
    ([] : operation list), Big_map.add k v s
end

let test =
  let init = Big_map.add 12 42n Big_map.empty in
  let _ = Test.originate (contract_of C) init 0tez in
  let init = Big_map.add 32 42n Big_map.empty in
  let orig = Test.originate (contract_of C) init 0tez in
  let ctr = Test.to_contract orig.addr in
  let m_old = Test.get_storage orig.addr in
  let () = Test.log m_old in
  let () = Test.log (Big_map.find_opt 21 m_old) in
  let _ = Test.transfer_to_contract_exn ctr (Main (21, 42n)) 0tez in
  let _ = Test.transfer_to_contract_exn ctr (Main (3, 42n)) 0tez in
  let m_new = Test.get_storage orig.addr in
  let () = Test.log m_old in
  let () = Test.log m_new in
  let () = Test.log (Big_map.find_opt 21 m_old) in
  let () = Test.log (Big_map.find_opt 21 m_new) in
  ()
