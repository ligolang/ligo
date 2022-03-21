type storage is unit
type return is list (operation) * storage

function cb (const s : storage) : return is block {
  const c : contract (unit) = Tezos.get_contract_with_error (Tezos.sender, "error")
} with (list [Tezos.transaction (unit, 0tez, c)], s)


function cbo (const s : unit) : return is
  block {
    const c : contract (unit) = Tezos.get_contract_with_error(Tezos.sender, "contract not found")
  } with (list [Tezos.transaction (unit, 0tez, c)], s)
