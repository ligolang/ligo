(* Test whether closures retain values in CameLIGO *)

let test (k: int) : int =
  let j: int = k + 5 in
  let close: (int -> int) =
    fun (i: int) -> i + j
  in
  let j: int = 20 in (* Shadow original variable to see if value close'd *)
  close 20
