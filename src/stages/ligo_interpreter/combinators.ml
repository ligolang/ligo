open Types

let v_pair : value * value -> value =
  fun (a,b) -> V_Record (LMap.of_list [(Label "0", a) ; (Label "1",b)])

let v_bool : bool -> value =
  fun b -> V_Ct (C_bool b)

let v_unit : unit -> value =
  fun () -> V_Ct (C_unit)

let v_string : string -> value =
  fun s -> V_Ct (C_string s)

let v_some : value -> value =
  fun v -> V_Construct ("Some", v)

let v_none : unit -> value =
  fun () -> V_Construct ("None", v_unit ())

let v_ctor : string -> value -> value =
  fun ctor value -> V_Construct (ctor, value)

let v_address : Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t -> value =
  fun a -> V_Ct (C_address a)

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

let counter_of_address : string -> int = fun addr ->
  try (int_of_string addr) with | Failure _ -> -1

let get_address : value -> Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.t option = function
  | V_Ct ( C_address x ) -> Some x
  | _ -> None

let get_michelson_contract : value -> unit Tezos_utils.Michelson.michelson option = function
  | V_Michelson ( Contract x ) -> Some x
  | _ -> None

let get_michelson_expr : value -> (unit Tezos_utils.Michelson.michelson * unit Tezos_utils.Michelson.michelson * Ast_typed.type_expression) option =
  function
  | V_Michelson ( Ty_code x ) -> Some x
  | _ -> None

(* let get_michelson_subst : value -> (unit Tezos_utils.Michelson.michelson * Ast_core.expression) option =
  function
  | V_Michelson ( Subst_code x ) -> Some x
  | _ -> None *)

let get_nat : value -> Z.t option =
  function
  | V_Ct ( C_nat x) -> Some x
  | _ -> None

let get_nat_option : value -> z option option =
  function
  | V_Construct ("Some", V_Ct (C_nat x)) -> Some (Some x)
  | V_Construct ("None", V_Ct C_unit) -> Some (None)
  | _ -> None

let get_int : value -> Z.t option =
  function
  | V_Ct ( C_int x) -> Some x
  | _ -> None

let get_string : value -> string option =
  function
  | V_Ct (C_string x) -> Some x
  | _ -> None

let get_string_option : value -> string option option =
  function
  | V_Construct ("Some", V_Ct (C_string x)) -> Some (Some x)
  | V_Construct ("None", V_Ct C_unit) -> Some (None)
  | _ -> None

let get_list : value -> 'a list option =
  fun value ->
    match value with
    | V_List lst -> Some lst
    | _ -> None

let get_pair : value -> (value * value) option =
  fun value ->
    match value with
    | V_Record lm -> (
      let x = LMap.to_kv_list lm in
      match x with
      | [ (Label "0", x ) ; (Label "1", y) ] -> Some (x,y)
      | _ -> None
    )
    | _ -> None
