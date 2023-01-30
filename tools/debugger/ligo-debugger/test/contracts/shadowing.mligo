let addOption (a, b : int option * int option) : int option =
  begin
    match a with
    | Some x ->
        begin
          match b with
          | Some y -> Some (x + y)
          | None -> a
        end
    | None -> b
  end

let main (_, s : unit * int) : operation list * int =
  let s1 = s + s in
  let s1 = Some (s1 * 2) in
  let s2 = Some (s + 28) in
  let s2 = addOption(s1, s2) in
  let s2 = addOption(s2, s2) in
  let res =
    match s2 with
    | Some s -> s + 10
    | None -> 42
  in (([] : operation list), res)
