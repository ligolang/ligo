let my_map : (int, string) big_map =
  Big_map.literal [(1,"one"); (2,"two")]
let map_with_3 = Big_map.update 3 (Some "three") my_map
let contains_3 = Big_map.mem 3 map_with_3 // = true
let map_without_2 = Big_map.update 2 None my_map
let contains_2 = Big_map.mem 2 map_without_2 // = false
// three = Some "three"
let three, map_without_3 = Big_map.get_and_update 3 None map_with_3