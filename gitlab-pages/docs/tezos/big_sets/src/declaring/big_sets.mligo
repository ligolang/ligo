type 'elt t = ('elt, unit) big_map
let empty_big_set : int big_set = Big_set.empty
let big_set1 : int big_set = Big_set.literal [3; 2; 2; 1]
let two = 2
let big_set2 : int big_set = Big_set.of_list [3; two; two; 1]