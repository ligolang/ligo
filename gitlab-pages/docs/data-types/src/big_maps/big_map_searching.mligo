let my_big_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let contains_2 : bool = Big_map.mem 2 my_big_map // = true
let value_option : string option = Big_map.find_opt 2 my_big_map
let value = match value_option with
    Some value -> value
  | None -> failwith "No value."