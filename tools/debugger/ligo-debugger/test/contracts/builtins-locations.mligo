[@entry]
let main (_, s : unit * int) : operation list * int =
  let v = is_nat s in
  let () = assert true in
  let res =
    match v with
    | Some _ -> s + 42
    | None -> s
  in
  let sum = List.fold (fun (acc, cur) -> acc + cur) [1; 2; 3] 0 in
  (([] : operation list), res + sum)
