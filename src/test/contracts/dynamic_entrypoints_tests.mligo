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

let get_storage = Test.Typed_address.get_storage
let transfer_exn = Test.Typed_address.transfer_exn

let test_dyn =
  let init_storage = Test.Dynamic_entrypoints.storage (contract_of C) 42 in
  let orig = Test.Originate.contract (contract_of C) init_storage 0mutez in
  (* Call initial one *)
  let _ = transfer_exn orig.taddr (Call_one ()) 1mutez in
  let () = Assert.assert ((get_storage orig.taddr).storage = 1) in
  (* Change initial one and call it *)
  let f = fun () (i : int) : operation list * int -> [], i + 1 in
  let _ = transfer_exn orig.taddr (Set_one f) 1mutez in
  let _ = transfer_exn orig.taddr (Call_one ()) 1mutez in
  Assert.assert ((get_storage orig.taddr).storage = 2)
