let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let new_map = Big_map.remove 2 my_map
let contains_3 = Big_map.mem 2 new_map // = false