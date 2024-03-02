let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Map.mem 2 my_map // = true
let v : string option = Map.find_opt 2 my_map
let force_access key map =
  match Map.find_opt key map with
    Some value -> value
  | None -> failwith "No value."