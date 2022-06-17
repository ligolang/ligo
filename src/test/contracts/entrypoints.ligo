type storage is unit

type return is list (operation) * storage

function cb (const a : address; const s : storage) : return is
  {
    const c : contract (unit) = Tezos.get_entrypoint ("%cb", a)
  } with (list [Tezos.transaction (unit, 0tez, c)], s)


function cbo (const a : address; const s : storage) : return is
  {
    const c : contract (unit) =
      case (Tezos.get_entrypoint_opt ("%cbo", a) : option (contract (unit))) of [
        Some (c) -> c
      | None -> (failwith ("cbo: Entrypoint not found.") : contract (unit))
      ]
  } with (list [Tezos.transaction (unit, 0tez, c)], s)
