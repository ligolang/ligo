let my_big_set : int big_set = Big_set.literal [3; 2; 2; 1]
let new_big_set = Big_set.remove 3 my_big_set
let contains_3 = Big_set.mem 3 new_big_set // = false