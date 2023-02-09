open Types
module Snippet = Simple_utils.Snippet
module PP_helpers = Simple_utils.PP_helpers
open PP_helpers
open Ligo_prim

let pp_ct : Format.formatter -> constant_val -> unit =
 fun ppf c ->
  match c with
  | C_unit -> Format.fprintf ppf "()"
  | C_bool t -> Format.fprintf ppf "%b" t
  | C_int z -> Format.fprintf ppf "%s" (Z.to_string z)
  | C_int64 z -> Format.fprintf ppf "%s" (Int64.to_string z)
  | C_nat n -> Format.fprintf ppf "%sn" (Z.to_string n)
  | C_timestamp t ->
    Format.fprintf
      ppf
      "timestamp(%s)"
      Memory_proto_alpha.Protocol.Time_repr.(to_notation @@ of_seconds (Z.to_int64 t))
  | C_string s -> Format.fprintf ppf "\"%s\"" s
  | C_bytes b -> Format.fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | C_address c ->
    Format.fprintf ppf "%a" Tezos_protocol.Protocol.Alpha_context.Contract.pp c
  | C_contract c ->
    Format.fprintf
      ppf
      "%a(%a)"
      Tezos_protocol.Protocol.Alpha_context.Contract.pp
      c.address
      (PP_helpers.option PP_helpers.string)
      c.entrypoint
  | C_mutez n -> Format.fprintf ppf "%smutez" (Z.to_string n)
  | C_key_hash c -> Format.fprintf ppf "%a" Tezos_crypto.Signature.Public_key_hash.pp c
  | C_key c -> Format.fprintf ppf "%a" Tezos_crypto.Signature.Public_key.pp c
  | C_signature s -> Format.fprintf ppf "%a" Tezos_crypto.Signature.pp s
  | C_bls12_381_g1 b ->
    Format.fprintf ppf "%s" (Bytes.to_string (Bls12_381.G1.to_bytes b))
  | C_bls12_381_g2 b ->
    Format.fprintf ppf "%s" (Bytes.to_string (Bls12_381.G2.to_bytes b))
  | C_bls12_381_fr b ->
    Format.fprintf ppf "%s" (Bytes.to_string (Bls12_381.Fr.to_bytes b))
  | C_chain_id c -> Format.fprintf ppf "%s" (Bytes.to_string (Chain_id.to_bytes c))


let rec pp_value ~no_colour : Format.formatter -> value -> unit =
  let pp_value ppf v = pp_value ~no_colour ppf v in
  let snippet_pp = Snippet.pp ~no_colour in
  fun ppf v ->
    match v with
    | V_Ct c -> Format.fprintf ppf "%a" pp_ct c
    | V_Func_val _ -> Format.fprintf ppf "<fun>"
    | V_Construct (name, v) -> Format.fprintf ppf "%s (%a)" name pp_value v
    | V_List vl -> Format.fprintf ppf "[%a]" (list_sep pp_value (tag " ; ")) vl
    | V_Set sl -> Format.fprintf ppf "{%a}" (list_sep pp_value (tag " ; ")) sl
    | V_Map vmap ->
      let aux : Format.formatter -> value * value -> unit =
       fun ppf (k, v) -> Format.fprintf ppf "%a -> %a" pp_value k pp_value v
      in
      Format.fprintf ppf "[%a]" (list_sep aux (tag " ; ")) vmap
    | V_Record recmap ->
      if Record.is_tuple recmap
      then (
        let aux : Format.formatter -> value -> unit =
         fun ppf v -> Format.fprintf ppf "%a" pp_value v
        in
        Format.fprintf ppf "(%a)" (list_sep aux (tag " , ")) (Record.values recmap))
      else (
        let aux : Format.formatter -> Label.t * value -> unit =
         fun ppf (Label l, v) -> Format.fprintf ppf "%s = %a" l pp_value v
        in
        Format.fprintf ppf "{%a}" (list_sep aux (tag " ; ")) (Record.to_list recmap))
    | V_Michelson (Ty_code { micheline_repr = { code; _ }; _ } | Untyped_code code) ->
      Format.fprintf ppf "%a" Tezos_utils.Michelson.pp code
    | V_Michelson_contract code -> Format.fprintf ppf "%a" Tezos_utils.Michelson.pp code
    | V_Ast_contract { main; views = _ } ->
      Format.fprintf ppf "%a" Ast_aggregated.PP.expression main
    | V_Mutation (l, _, s) ->
      Format.fprintf ppf "Mutation at: %a@.Replacing by: %s.@." snippet_pp l s
    | V_Gen _ -> Format.fprintf ppf "Generator"
    | V_Location _ -> Format.fprintf ppf "Heap location"
    | V_Typed_address c ->
      Format.fprintf ppf "%a" Tezos_protocol.Protocol.Alpha_context.Contract.pp c


let pp_value_expr ~no_colour : Format.formatter -> value_expr -> unit =
 fun ppf v -> Format.fprintf ppf "%a" (pp_value ~no_colour) v.eval_term


let pp_env ~no_colour : Format.formatter -> env -> unit =
 fun ppf env ->
  let aux : Format.formatter -> Value_var.t * env_item -> unit =
   fun ppf -> function
    | name, { item; no_mutation = _; inline = _ } ->
      Format.fprintf ppf "%a -> %a" Value_var.pp name (pp_value_expr ~no_colour) item
  in
  Format.fprintf
    ppf
    "@[<v 2>%i bindings in environment:@ %a@]"
    (List.length env)
    (list_sep aux (tag "@ "))
    env
