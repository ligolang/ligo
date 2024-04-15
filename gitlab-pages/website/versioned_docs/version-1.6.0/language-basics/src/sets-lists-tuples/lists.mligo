let empty_list : int list = []
let my_list : int list = [1; 2; 2] (* The head is 1, the tail is [2; 2] *)
let larger_list : int list = 5 :: my_list (* [5;1;2;2] *)
let head : int option = List.head_opt my_list (* 1 *)
let tail : int list option = List.tail_opt my_list (* [2;2] *)
let assert_all_greater_than_three (l : int list) : unit =
  let predicate = fun (i:int) -> assert (i > 3)
  in List.iter predicate l
let increment (i : int) = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list (* [6,2,3,3] *)
let sum (acc, i: int * int) = acc + i
let sum_of_elements : int = List.fold_left sum 0 my_list