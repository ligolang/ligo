let my_list : int list = [1; 2; 3]
let head_option : int option = List.head my_list
let head = match head_option with
| Some value -> value
| None -> failwith "Failed to get the head of the list"
let tail_option : int list option = List.tail my_list
let tail = match tail_option with
| Some value -> value
| None -> failwith "Failed to get the tail of the list"