open Types

let v_pair : value * value -> value =
  fun (a,b) -> V_Record (LMap.of_list [(Label "0", a) ; (Label "1",b)])

let v_bool : bool -> value =
  fun b -> V_Ct (C_bool b)

let v_unit : unit -> value =
  fun () -> V_Ct (C_unit)

let v_some : value -> value =
  fun v -> V_Construct ("Some", v)

let v_none : unit -> value =
  fun () -> V_Construct ("None", v_unit ())

let extract_pair : value -> (value * value) option =
  fun p ->
    ( match p with
      | V_Record lmap ->
        let fst = LMap.find (Label "0") lmap in
        let snd = LMap.find (Label "1") lmap in
        Some (fst,snd)
      | _ -> None
    )

let extract_fold_while_result : value -> (bool * value) option =
  fun p ->
    match extract_pair p with
    | Some (V_Ct (C_bool a),b) -> Some (a,b)
    | _ -> None

let is_true : value -> bool =
  fun b -> match b with
    | V_Ct (C_bool true) -> true
    | _ -> false

let is_bool : value -> bool =
  fun b -> match b with
    | V_Ct (C_bool _) -> true
    | _ -> false

let get_operations : Types.value -> Types.operation list = fun contract_out ->
  match contract_out with
    | V_Record v -> ( match Types.LMap.find_opt (Label "0") v with
      | Some (V_List list_op) ->
        let aux : Types.value -> Types.operation = fun v -> match v with
          | V_Ct (C_operation op) -> op
          | _ -> failwith "ill-typed contract" in
        List.map aux list_op
      | _ ->
        failwith "ill-typed contract"
    )
    | _ -> failwith "ill-typed contract"

let counter_of_address : string -> int = fun addr ->
  try (int_of_string addr) with | Failure _ -> -1