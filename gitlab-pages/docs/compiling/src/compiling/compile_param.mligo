module ComplexParam = struct
  type storage = int
  type return_type = operation list * storage
  type paramType = (int list * int list * (string * string))

  let sum_fold = fun (result, i: int * int) -> result + i
  let mult_fold = fun (result, i: int * int) -> result * i

  [@entry]
  let complexmath (param : paramType) (_s : storage) : return_type =
    let list1, list2, str_tuple = param in
    let sum : int = List.fold sum_fold list1 0 in
    let product : int = List.fold mult_fold list2 1 in
    let str1, str2 = str_tuple in
    let string_diff : int = String.length(str2) - String.length(str1) in
    let newVal = (sum + product) * string_diff in
    [], newVal

end