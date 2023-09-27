module C = struct
  type storage =
    {
     storage : int;
     dynamic_entrypoints
    }

  [@dyn_entry]
  let one () (_ : int) : operation list * int = [], 1

  [@entry]
  let call_one () (s : storage) : operation list * storage =
    match Dynamic_entrypoints.get one s.dynamic_entrypoints with
      Some f ->
        let op, storage = f () s.storage in
        op, {s with storage}
    | None -> failwith (-1)

  [@entry]
  let set_one (one_v2 : (unit, int) entrypoint) (s : storage)
  : operation list * storage =
    let dynamic_entrypoints =
      Dynamic_entrypoints.set one (Some one_v2) s.dynamic_entrypoints in
    [], {s with dynamic_entrypoints}

end

let test_dyn =
  let init_storage = Test.storage_with_dynamic_entrypoints (contract_of  C) 42 in
  let orig = Test.originate (contract_of C) init_storage 0mutez in
  (* Call initial one *)
  let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
  let () = assert ((Test.get_storage orig.addr).storage = 1) in
  (* Change initial one and call it *)
  let f = fun () (i : int) : operation list * int -> [], i + 1 in
  let _ = Test.transfer_exn orig.addr (Set_one f) 1mutez in
  let _ = Test.transfer_exn orig.addr (Call_one ()) 1mutez in
  assert ((Test.get_storage orig.addr).storage = 2)
