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

let get_nat : value -> Z.t option =
  function
  | V_Ct ( C_nat x) -> Some x
  | _ -> None

let get_mutez : value -> Z.t option =
  function
  | V_Ct ( C_mutez x) -> Some x
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

let get_option : value -> value option option =
  fun value ->
    match value with
    | V_Construct ("Some", v) -> Some (Some v)
    | V_Construct ("None", _) -> Some None
    | _ -> None

let get_map : value -> (value * value) list option =
  fun value ->
    match value with
    | V_Map v -> Some v
    | _ -> None

let get_set : value -> value list option =
  fun value ->
    match value with
    | V_Set v -> Some v
    | _ -> None

let get_list : value -> value list option =
  fun value ->
    match value with
    | V_List v -> Some v
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

let get_func : value -> func_val option =
  fun value ->
    match value with
    | V_Func_val v -> Some v
    | _ -> None

let equal_constant_val (c : constant_val) (c' : constant_val) : bool =
  match c, c' with
  | C_unit, C_unit -> true
  | C_bool b, C_bool b' -> b = b'
  | C_int i, C_int i' -> Int.compare i i' = 0
  | C_nat n, C_nat n' -> Int.compare n n' = 0
  | C_timestamp t, C_timestamp t' -> Int.compare t t' = 0
  | C_string s, C_string s' -> String.equal s s'
  | C_bytes b, C_bytes b' -> Bytes.equal b b'
  | C_mutez m, C_mutez m' -> Int.compare m m' = 0
  | C_address a, C_address a' ->
     Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.equal a a'
  | C_contract {address=a;entrypoint=None}, C_contract {address=a';entrypoint=None} ->
     Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.equal a a'
  | C_contract {address=a;entrypoint=Some e}, C_contract {address=a';entrypoint=Some e'} ->
     Tezos_protocol_009_PsFLoren.Protocol.Alpha_context.Contract.equal a a' && String.equal e e'
  | C_key_hash kh, C_key_hash kh' ->
     Tezos_crypto.Signature.Public_key_hash.equal kh kh'
  | _, _ -> false

let rec equal_value (v : value) (v' : value) : bool =
  match v, v' with
  | V_Ct c, V_Ct c' -> equal_constant_val c c'
  | V_List l, V_List l' -> List.equal equal_value l l'
  | V_Construct (c, l), V_Construct (c', l') ->
     String.equal c c' && equal_value l l'
  | V_Record r, V_Record r' ->
     let r = LMap.to_kv_list r in
     let r' = LMap.to_kv_list r' in
     List.equal (fun (Label l, v) (Label l', v') -> String.equal l l' && equal_value v v') r r'
  | V_Set s, V_Set s' ->
     List.equal equal_value s s'
  | V_Map m, V_Map m' -> 
    List.equal (fun (k1, v1) (k2, v2) -> equal_value k1 k2 && equal_value v1 v2) m m'
  | _, _ -> false
