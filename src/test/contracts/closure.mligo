(* Test whether closures capture variables in CameLIGO *)

let test (k : int) : int =
  let j : int = k + 5 in
  let close : int -> int = fun (i : int) -> i + j in
  let j : int = 20 (* Shadow original variable *)
  in close 20
