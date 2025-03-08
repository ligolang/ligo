let xs : int list = [1; 2; 3]

let length : nat = List.length xs
let size : nat = List.size xs
let head_opt : int option = List.head_opt xs
let tail_opt : int list option = List.tail_opt xs
let iter_op (l : int list) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in List.iter predicate l
let larger_list: int list = [1; 2; 3]

let increment (i : int) : int = i + 1

// Creates a new list with all elements incremented by 1
let plus_one : int list = List.map increment larger_list
let my_list : int list = [1; 2; 3]

let sum (acc, i : int * int) : int = acc + i

let sum_of_elements : int = List.fold sum my_list 0
let my_list : int list = [1; 2; 3]

let sum (acc, i : int * int) : int = acc + i

let sum_of_elements : int = List.fold_left sum 0 my_list
let my_list : int list = [1; 2; 3]

let sum_right (i, acc : int * int) : int = acc + i

let sum_of_elements : int = List.fold_right sum_right my_list 0