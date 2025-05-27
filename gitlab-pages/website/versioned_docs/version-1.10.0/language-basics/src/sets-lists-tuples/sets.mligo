let my_set : int set = Set.empty
let my_set : int set = Set.literal [3; 2; 2; 1]
let with_999 : int set = Set.add 999 my_set
let contains_3 : bool = Set.mem 3 my_set
let cardinal : nat = Set.size my_set
let larger_set  : int set = Set.add 4 my_set
let smaller_set : int set = Set.remove 3 my_set
let assert_all_greater_than_three (s : int set) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in Set.iter predicate s
let sum (acc, i : int * int) : int = acc + i
let sum_of_elements : int = Set.fold sum my_set 0