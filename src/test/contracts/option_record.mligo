
type t = { s : int option ; n : string option } option

module C = struct
  type return = operation list * t
  [@entry]
  let main (_ : unit) (_ : t) : return = 
    ([] : operation list), Some { s = Some 1 ; n = (None : string option) }
end

let test =
  let orig = Test.originate (contract_of C) (None : t) 0tez in
  let ctr = Test.to_contract orig.addr in
  let _ = Test.transfer_to_contract_exn ctr (Main ()) 0tez in
  let v = Test.get_storage orig.addr in
  let v = Option.unopt v in
  let s = Option.unopt v.s in
  assert (s = 1)
