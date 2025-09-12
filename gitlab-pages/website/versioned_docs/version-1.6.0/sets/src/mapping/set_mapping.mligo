let s : int set = Set.literal [5; 1; 2; 2]
// plus_one = Set.literal [6; 2; 3]
let plus_one : int set = Set.map (fun i -> i + 1) s