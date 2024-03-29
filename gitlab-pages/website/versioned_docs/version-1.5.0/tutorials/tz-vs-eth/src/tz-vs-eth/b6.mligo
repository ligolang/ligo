let x : int option = Some 5

let x_or_zero =
  match x with
    Some value -> value
  | None -> 0