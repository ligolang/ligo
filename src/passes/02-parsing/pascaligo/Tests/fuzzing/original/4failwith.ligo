type parameter is
  Zero of nat
| Pos  of nat

type storage is unit

type return is list (operation) * storage

function main (const p : parameter; const s : storage) : return is {
  case p of [
    Zero (n) -> if n > 0n then failwith ("fail")
  | Pos (n)  -> if n = 0n then failwith ("fail")
  ]
} with ((nil : list (operation)), s)

function foobar (var i : int) : int is {
  var p : parameter := Zero (42n);
  if i > 0 then {
    i := i + 1;
    if i > 10 then {
      i := 20;
      failwith ("who knows");
      i := 30 // Should be no-op
    }
  }
  else {
    case p of [
      Zero (_) -> failwith (42n)
    | Pos (_)  -> skip
    ]
  }
} with
    case p of [
      Zero (_) -> i
    | Pos (_)  -> (failwith ("waaaa") : int)
    ]

function failer (const p : int) : int is {
  if p = 1 then failwith (42)
} with p
