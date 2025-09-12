let short_list = [1; 2; 2]
// long_list = [5; 1; 2; 2]
let long_list : int list = 5 :: short_list
// longer_list = [6; 5; 1; 2; 2]
let longer_list = List.cons 6 long_list