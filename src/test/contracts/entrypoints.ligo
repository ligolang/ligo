type storage is unit

type return is list (operation) * storage

function cb (const a : address; const s : storage) : return is
  block {
    const c : contract (unit) = get_entrypoint ("%cb", a)
  }
  with (list [transaction (unit, 0mutez, c)], s)


function cbo (const a : address; const s : storage) : return is
  block {
    const c : contract (unit) =
      case (get_entrypoint_opt ("%cbo", a) : option (contract (unit))) of
        Some (c) -> c
      | None -> (failwith ("entrypoint not found") : contract (unit))
      end
  } with (list [transaction(unit, 0mutez, c)], s)
