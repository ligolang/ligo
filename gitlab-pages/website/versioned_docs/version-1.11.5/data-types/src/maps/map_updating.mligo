let my_map : (int, string) map = Map.literal [(1,"one"); (2,"two")]
let map_with_3 = Map.update 3 (Some "three") my_map
let contains_3 = Map.mem 3 map_with_3 // = true
let map_without_2 = Map.update 2 None my_map
let contains_2 = Map.mem 2 map_without_2 // = false
// three = Some "three"
let three, map_without_3 = Map.get_and_update 3 None map_with_3