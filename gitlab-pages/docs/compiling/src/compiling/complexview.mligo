type storage_type = int
type return_type = operation list * storage_type

type view_param =
  int list
  * int set
  * int option

module Counter = struct

  [@entry]
  let add (value : int) (storage : storage_type) : return_type =
    [], storage + value

  [@entry]
  let sub (value : int) (storage : storage_type) : return_type =
    [], storage - value

  [@view]
  let math (param : view_param) (storage : storage_type) : int =
    let intList, intSet, intOption = param in

    (* Get the sum of the ints in the list *)
    let listSum = List.fold_left (fun (a, b) -> a + b) 0 intList in

    (* Multiply by the sum of the ints in the set *)
    let setSum = Set.fold (fun (a, b) -> a + b) intSet 0 in
    let returnValue = listSum * setSum in

    (* If the option int was provided, subtract it *)
    match intOption with
      None -> storage + returnValue
    | Some value -> storage + returnValue - value
end