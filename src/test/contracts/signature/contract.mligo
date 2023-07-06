type storage = string

module type IIncDec = sig
  type storage
  type return = operation list * storage

  val initial_storage : storage

  [@entry] val increment : int -> storage -> return
  [@entry] val decrement : int -> storage -> return
  [@entry] val reset : unit -> storage -> return
end

module IncDec : IIncDec = struct
  type storage = int
  type return = operation list * storage

  let initial_storage : storage = 42

  (* Three entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : return =
    [], store + delta
  [@entry] let decrement (delta : int) (store : storage) : return =
    [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : return =
    [], 0
end


module type IInc = sig
  type storage
  type return = operation list * storage

  val initial_storage : storage
  [@entry] val increment : int -> storage -> return
  [@entry] val reset : unit -> storage -> return
  [@entry] val one : unit -> storage -> return
end

module Inc : IInc = struct
  include IncDec

  [@entry] let one (() : unit) (_ : storage) : return = [], 1
end

let test =
  let taddr, _, _ = Test.originate_module (contract_of Inc) (Inc.initial_storage : Inc.storage) 0tez in
  let x : Inc parameter_of = One () in
  let () = Test.log x in
  let _ = Test.transfer_to_contract_exn (Test.to_contract taddr) x 0tez in
  let x : Inc parameter_of = Increment 41 in
  let () = Test.log x in
  let _ = Test.transfer_to_contract_exn (Test.to_contract taddr) x 0tez in
  Test.log (Test.get_storage taddr)

let test2 =
  let taddr, _, _ = Test.originate_module (contract_of IncDec) (IncDec.initial_storage : IncDec.storage) 0tez in
  let x : IncDec parameter_of = Decrement 42 in
  let () = Test.log x in
  let _ = Test.transfer_to_contract_exn (Test.to_contract taddr) x 0tez in
  Test.log (Test.get_storage taddr)
