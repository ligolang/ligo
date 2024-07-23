let my_map : (int, int) map = Map.literal [(0,0); (1,1); (2,2)]
// plus_one = Map.literal [(0,0); (1,2); (2,4)]
let plus_one = Map.map (fun (k,v) -> k + v) my_map