module Test = Test.Next

type t = { s : int option ; n : string option } option

module C = struct
  type return = operation list * t
  [@entry]
  let main (_ : unit) (_ : t) : return =
    ([] : operation list), Some { s = Some 1 ; n = (None : string option) }
end

let test =
  let orig = Test.Originate.contract (contract_of C) (None : t) 0tez in
  let ctr = Test.Typed_address.to_contract orig.taddr in
  let _ = Test.Contract.transfer_exn ctr (Main ()) 0tez in
  let v = Test.Typed_address.get_storage orig.taddr in
  let v = Option.value_with_error "option is None" v in
  let s = Option.value_with_error "option is None" v.s in
  Assert.assert (s = 1)
