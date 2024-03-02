let my_set : int set = Set.literal [3; 2; 2; 1]
let new_set = Set.remove 3 my_set
let contains_3 = Set.mem 3 new_set // = false