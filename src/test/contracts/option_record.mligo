type t = { s : int option ; n : string option } option

type return = operation list * t

let main (_ : unit * t) : return = 
  ([] : operation list), Some { s = Some 1 ; n = (None : string option) }

let test =
  let (taddr, _, _) = Test.originate_uncurried main (None : t) 0tez in
  let ctr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn ctr () 0tez in
  let v = Test.get_storage taddr in
  let v = Option.unopt v in
  let s = Option.unopt v.s in
  assert (s = 1)
