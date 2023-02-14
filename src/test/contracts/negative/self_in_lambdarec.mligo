let rec foo (n : int) : address =
  if (n <= 1) then
    Tezos.address (Tezos.self "%default" : int contract)
  else
    let addr = foo (n - 1) in
    Tezos.address (Option.unopt (Tezos.get_contract_opt addr : int contract option))

let main ((p, _s) : int * address): (operation list * address) =
  let _dummy = foo p in (* force not to inline foo *)
  ( ([] : operation list) , foo p)
