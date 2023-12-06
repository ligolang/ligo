let my_set : int set = Set.empty
let my_set : int set = Set.literal [3; 2; 2; 1]
let contains_3 : bool = Set.mem 3 my_set
let card : nat = Set.cardinal my_set
let updated_set = Set.add 4 my_set
let updated_set = Set.remove 3 my_set
(* in case of true value will be added to the set *)
let updated_set = Set.update 4 true my_set

(* in case of false value will be removed from the set *)
let updated_set = Set.update 4 false my_set
let iter_op (s : int set) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in Set.iter predicate s
let sum (acc, i : int * int) : int = acc + i
let sum_of_elements : int = Set.fold sum my_set 0
let sum_right (i, acc : int * int) : int = acc + i
let sum_of_elements : int = Set.fold_desc sum_right my_set 0