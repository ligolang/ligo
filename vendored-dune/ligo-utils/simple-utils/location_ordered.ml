type t = Location.t

let compare a b =
  let open Location in
  match a,b with
  | (File a, File b) -> (
    let pos_compare x y =
      if Pos.equal x y then 0
      else if Pos.lt x y then -1
      else 1
    in
    let file_cmp = String.compare a#file b#file in
    let start_cmp = pos_compare a#start b#start in
    let stop_cmp = pos_compare a#stop b#stop in
    match file_cmp, start_cmp, stop_cmp with
    | 0, 0, c
    | 0, c, _
    | c, _, _ -> c
    )
  | (File _, Virtual _) -> -1
  | (Virtual _, File _) -> 1
  | (Virtual a, Virtual b) -> String.compare a b

let equal a b = (compare a b) = 0