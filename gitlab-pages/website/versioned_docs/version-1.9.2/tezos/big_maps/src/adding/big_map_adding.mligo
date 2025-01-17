let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let new_map = Big_map.add 3 "three" my_map
let contains_3 = Big_map.mem 3 new_map // = true