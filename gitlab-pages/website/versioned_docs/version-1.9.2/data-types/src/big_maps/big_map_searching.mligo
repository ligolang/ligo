let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Big_map.mem 2 my_map // = true
let v : string option = Big_map.find_opt 2 my_map
let force_access key map =
  match Big_map.find_opt key map with
    Some value -> value
  | None -> failwith "No value."