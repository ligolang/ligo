const my_list : list (int) = list [1; 2; 2] // The head is 1

const larger_list : int_list = 5 # my_list

function increment (const i : int): int is i + 1

// Creates a new list with all elements incremented by 1
const plus_one : list (int) = list_map (increment, larger_list);

function sum (const acc : int; const i : int): int is acc + i

const sum_of_elements : int = list_fold (sum, my_list, 0)
