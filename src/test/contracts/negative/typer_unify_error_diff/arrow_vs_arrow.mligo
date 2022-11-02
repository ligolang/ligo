
(*
  In this case, the typer will stop at the first mismatch
  between arrow components.
  In below example, it will fail at [nat] vs. [int].

  TODO : How can we make the error message more precise
  and pinpoint a clear diff between both arrow types ?
*)
type a = int -> nat -> nat -> tez
type b = int -> int -> int -> int -> nat

let main (_p, s : int * int) : operation list * int =
  let  x : a = (fun _x _y _z -> 1tez) in
  let _y : b = x in
  ([] : operation list), s
