module Test = Test.Next

module C = struct
  type storage = (int, nat -> nat) big_map
  [@entry]
  let main ((k,v) : int * (nat -> nat)) (s : storage) : operation list * storage =
    ([] : operation list), Big_map.add k v s
end

let test =
  let {taddr; code = _; size = _} = Test.Originate.contract (contract_of C) Big_map.empty 0tez in
  let y : nat = 1n in
  let _ = Test.Typed_address.transfer_exn taddr (Main (21, (fun (x : nat) -> x * 2n + y))) 0tez in
  let _y : nat = 100n in
  let init = Big_map.add 21 (fun (_ : nat) -> 0n) (Big_map.empty : (int, nat -> nat) big_map) in
  let () = Test.State.set_big_map 5 init in
  let m_new = Test.Typed_address.get_storage taddr in
  let v = Big_map.find_opt 21 m_new in
  match v with
  | Some f ->
      let () = Test.IO.log (f 4n) in
      let () = Test.State.set_big_map 4 init in
      let m_new = Test.Typed_address.get_storage taddr in
      let v = Big_map.find_opt 21 m_new in
      (match v with
       | Some f ->
           Test.IO.log (f 4n)
       | None ->
           Test.IO.log "Error")
  | None ->
      Test.IO.log "Error"
