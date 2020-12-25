open Types
open Simple_utils.PP_helpers

let pp_ct : Format.formatter -> constant_val -> unit = fun ppf c ->
  match c with
  | C_unit -> Format.fprintf ppf "()"
  | C_bool t -> Format.fprintf ppf "%b" t
  | C_int z -> Format.fprintf ppf "%s" (Int.to_string z)
  | C_nat n -> Format.fprintf ppf "%sn" (Int.to_string n)
  | C_timestamp t -> Format.fprintf ppf "timestamp(%a)" Z.pp_print t
  | C_mutez m -> Format.fprintf ppf "%Ld mutez" (Tez.to_mutez m)
  | C_string s -> Format.fprintf ppf "\"%s\"" s
  | C_bytes b -> Format.fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | C_address s -> Format.fprintf ppf "@%s" s
  | C_signature s -> Format.fprintf ppf "signature %s" s
  | C_key s -> Format.fprintf ppf "key %s" s
  | C_key_hash s -> Format.fprintf ppf "key_hash %s" s
  | C_chain_id s -> Format.fprintf ppf "chain_id %s" s
  | C_operation _ -> Format.fprintf ppf "operation"

let rec pp_value : Format.formatter -> value -> unit = fun ppf v ->
  match v with
  | V_Ct c -> Format.fprintf ppf "%a" pp_ct c
  | V_packed c -> Format.fprintf ppf "packed data %a" pp_value c
  | V_Failure s -> Format.fprintf ppf "Failure(\"%s\")" s
  | V_Func_val _ -> Format.fprintf ppf "<fun>"
  | V_Func_rec _ -> Format.fprintf ppf "<rec fun>"
  | V_Construct (name,v) -> Format.fprintf ppf "%s (%a)" name pp_value v
  | V_List vl -> Format.fprintf ppf "[%a]" (list_sep pp_value (tag " ; ")) vl
  | V_Set sl -> Format.fprintf ppf "{%a}" (list_sep pp_value (tag " ; ")) sl
  | V_Map vmap ->
    let aux : Format.formatter -> (value * value) -> unit = fun ppf (k, v) ->
      Format.fprintf ppf "%a -> %a" pp_value k pp_value v
    in
    Format.fprintf ppf "[%a]" (list_sep aux (tag " ; ")) vmap
  | V_Record recmap  ->
    if (Ast_typed.Helpers.is_tuple_lmap recmap) then
      let aux : Format.formatter -> value -> unit = fun ppf v ->
        Format.fprintf ppf "%a" pp_value v
      in
      Format.fprintf ppf "(%a)" (list_sep aux (tag " , ")) (LMap.to_list recmap)
    else
      let aux : Format.formatter -> (label * value) -> unit = fun ppf (Label l, v) ->
        Format.fprintf ppf "%s = %a" l pp_value v
      in
      Format.fprintf ppf "{%a}" (list_sep aux (tag " ; ")) (LMap.to_kv_list recmap)
    
let pp_context : Format.formatter -> Mini_proto.t -> unit = fun ppf { contracts ; step_constants=_ } ->
  let open Mini_proto in
  let aux : Format.formatter -> addr * state -> unit = fun ppf (Counter addr, {script = { code ; storage } ; script_balance }) ->
    Format.fprintf ppf "@[<h>{@ Address %d:@ code : %a@ storage : %a@ script_balance %a@ }@]"
      addr
      pp_value code
      pp_value storage
      Tez.pp script_balance
  in
  Format.fprintf ppf "@[<h 2>%a@]"
    (list_sep aux (tag "@ "))
    (StateMap.to_kv_list contracts)

let pp_env : Format.formatter -> env -> unit = fun ppf env ->
  let aux : Format.formatter -> expression_variable * value -> unit = fun ppf (var,v) ->
    Format.fprintf ppf "%a -> %a" Var.pp var.wrap_content pp_value v in
  Format.fprintf ppf "@[<v 2>%i bindings in environment:@ %a@]"
    (Env.cardinal env)
    (list_sep aux (tag "@ "))
    (Env.to_kv_list env)
