module IncDec = struct
  type storage = int
  type return = operation list * storage

  (* Three entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : return =
    [], store + delta
  [@entry] let decrement (delta : int) (store : storage) : return =
    [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : return =
    [], 0
end

(* Tests for main access point *)

let initial_storage = 42

let test_initial_storage =
 let (taddr, _, _) = Test.originate_module (contract_of IncDec) initial_storage 0tez in
 assert (Test.get_storage taddr = initial_storage)

let test_increment =
 let (taddr, _, _) = Test.originate_module (contract_of IncDec) initial_storage 0tez in
 let contr = Test.to_contract taddr in
 let _ = Test.transfer_to_contract_exn contr (Increment 1) 1mutez in
 assert (Test.get_storage taddr = initial_storage + 1)
