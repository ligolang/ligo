type param is
| Zero of nat
| Pos of nat

function main (const p : param; const s : unit) : list(operation) * unit is
  block {
    case p of
    | Zero (n) -> if n > 0n then failwith("fail") else skip
    | Pos (n) -> if n > 0n then skip else failwith("fail")
    end
  }
  with ((nil : list(operation)), s)
