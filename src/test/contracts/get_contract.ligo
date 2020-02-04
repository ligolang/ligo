function cb(const s : unit) : list(operation) * unit is
  block {
    const c : contract(unit) = get_contract(source)
  }
  with (list transaction(unit, 0mutez, c) end, s)


function cbo(const s : unit) : list(operation) * unit is
  block {
    const c : contract(unit) = 
      case (get_contract_opt(source) : option(contract (unit))) of
        | Some (c) -> c
        | None -> (failwith ("contract not found") : contract (unit))
      end
  }
  with (list transaction(unit, 0mutez, c) end, s)
