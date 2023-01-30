type 'l interface = { 
    data      : 'l; 
    supply    : 'l -> nat -> nat
}

module Impl = struct 
  type l = {
    values: (address,nat) big_map;
    supply: nat
  }

  let supply (ledger:l) (_:nat) = 
    ledger.supply

  let ledger_module (data: l) : l interface = { 
    data; supply
  }
end


let main (_ : unit) (storage: Impl.l) : operation list * Impl.l = 
  let ledger_module = Impl.ledger_module storage in
  let _ = ledger_module.supply ledger_module.data 0n in
  [], storage
