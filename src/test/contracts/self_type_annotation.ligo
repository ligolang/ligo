type parameter is nat
type storage is int
type return is list (operation) * storage


function main (const p : parameter; const s : storage) : return is
  block {
    const self_contract: contract(parameter) = Tezos.self ;
  }
  with ((nil: list(operation)), s)