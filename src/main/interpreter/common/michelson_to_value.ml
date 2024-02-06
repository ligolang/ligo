open Simple_utils.Trace
open Ligo_interpreter.Types
open Tezos_micheline.Micheline
open Ligo_prim

let contract_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse address")
  @@ Tezos_protocol.Protocol.Alpha_context.Contract.of_b58check s


let contract_of_bytes ~raise b =
  Proto_alpha_utils.Trace.trace_option
    ~raise
    (Errors.generic_error Location.generated "Cannot parse address")
  @@ Data_encoding.Binary.of_bytes_opt
       Memory_proto_alpha.Protocol.Alpha_context.Contract.encoding
       b


let key_hash_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse key_hash")
  @@ Tezos_crypto.Signature.Public_key_hash.of_b58check s


let key_hash_of_bytes ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse key_hash")
  @@ Tezos_crypto.Signature.Public_key_hash.of_bytes s


let key_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse key")
  @@ Tezos_crypto.Signature.Public_key.of_b58check s


let key_of_bytes ~raise s =
  Proto_alpha_utils.Trace.trace_option
    ~raise
    (Errors.generic_error Location.generated "Cannot parse key")
  @@ Tezos_crypto.Signature.Public_key.of_bytes_without_validation s


let bls12_381_g1_of_bytes ~raise s =
  Proto_alpha_utils.Trace.trace_option
    ~raise
    (Errors.generic_error Location.generated "Cannot parse bls12_381_g1")
  @@ Bls12_381_G1.of_bytes_opt s


let bls12_381_g2_of_bytes ~raise s =
  Proto_alpha_utils.Trace.trace_option
    ~raise
    (Errors.generic_error Location.generated "Cannot parse bls12_381_g2")
  @@ Bls12_381_G2.of_bytes_opt s


let bls12_381_fr_of_bytes ~raise s =
  Proto_alpha_utils.Trace.trace_option
    ~raise
    (Errors.generic_error Location.generated "Cannot parse bls12_381_fr")
  @@ Bls12_381_Fr.of_bytes_opt s


let signature_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse signature")
  @@ Tezos_crypto.Signature.of_b58check s


let chain_id_of_bytes ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse chain_id")
  @@ Tezos_crypto.Hashed.Chain_id.of_bytes s


let chain_id_of_string ~raise s =
  Proto_alpha_utils.Trace.trace_tzresult ~raise (fun _ ->
      Errors.generic_error Location.generated "Cannot parse chain_id")
  @@ Tezos_crypto.Hashed.Chain_id.of_b58check s


let wrong_mini_c_value _t _v =
  Errors.generic_error Location.generated "wrong_mini_c_value"


let corner_case ~loc s =
  ignore loc;
  Errors.generic_error Location.generated @@ "corner_case: " ^ s


let untranspilable t v =
  let v =
    v
    |> Tezos_micheline.Micheline.map_node
         (fun _ -> { Tezos_micheline.Micheline_printer.comment = None })
         (fun x -> x)
  in
  let t =
    t
    |> Tezos_micheline.Micheline.map_node
         (fun _ -> { Tezos_micheline.Micheline_printer.comment = None })
         (fun x -> x)
  in
  let s =
    Format.asprintf
      " %a %a"
      Tezos_micheline.Micheline_printer.print_expr
      t
      Tezos_micheline.Micheline_printer.print_expr
      v
  in
  Errors.generic_error Location.generated ("untranspilable" ^ s)


let rec comb prim loc xs =
  match xs with
  | [] | [ _ ] -> assert false
  | [ x1; x2 ] -> Prim (loc, prim, [ x1; x2 ], [])
  | x1 :: x2 :: xs ->
    let xs = comb prim loc (x2 :: xs) in
    Prim (loc, prim, [ x1; xs ], [])


let normalize_edo_comb_type = function
  | Prim (loc, "pair", xs, _) -> comb "pair" loc xs
  | t -> t


let normalize_edo_comb_value = function
  (* only do it for type is "pair" (and "ticket"), because Seq case is ambiguous *)
  | Prim (_, "pair", _, _) | Prim (_, "ticket", _, _) ->
    (function
    | Prim (loc, "Pair", xs, _) -> comb "Pair" loc xs
    | Seq (loc, xs) -> comb "Pair" loc xs
    | x -> x)
  | _ -> fun x -> x


let rec decompile_to_untyped_value ~raise ~bigmaps
    : ('l, string) node -> ('l, string) node -> Ligo_interpreter.Types.value
  =
 fun ty value ->
  let ty = normalize_edo_comb_type ty in
  let value = normalize_edo_comb_value ty value in
  let loc = Location.interpreter in
  match ty, value with
  | Prim (_, "pair", ts, _), Prim (_, "Pair", vs, _) ->
    let els =
      List.map
        ~f:(fun (t, v) -> decompile_to_untyped_value ~raise ~bigmaps t v)
        (List.zip_exn ts vs)
    in
    let rec aux l : value =
      match l with
      | [] -> raise.error (untranspilable ty value)
      | [ x ] -> x
      | hd :: tl ->
        let tl' = aux tl in
        Ligo_interpreter.Combinators.v_pair (hd, tl')
    in
    aux els
  | Prim (_, "or", [ a_ty; _ ], _), Prim (_, "Left", [ a ], _) ->
    let a = decompile_to_untyped_value ~raise ~bigmaps a_ty a in
    V_Construct ("Left", a)
  | Prim (_, "or", [ _; b_ty ], _), Prim (_, "Right", [ b ], _) ->
    let b = decompile_to_untyped_value ~raise ~bigmaps b_ty b in
    V_Construct ("Right", b)
  | Prim (_, "int", [], _), Int (_, n) -> V_Ct (C_int n)
  | Prim (_, "nat", [], _), Int (_, n) -> V_Ct (C_nat n)
  | Prim (_, "chain_id", _, _), String (_, id) ->
    V_Ct (C_chain_id (chain_id_of_string ~raise id))
  | Prim (_, "chain_id", _, _), Bytes (_, b) ->
    V_Ct (C_chain_id (chain_id_of_bytes ~raise b))
  | Prim (_, "key_hash", [], _), String (_, n) ->
    V_Ct (C_key_hash (key_hash_of_string ~raise n))
  | Prim (_, "key_hash", [], _), Bytes (_, b) ->
    V_Ct (C_key_hash (key_hash_of_bytes ~raise b))
  | Prim (_, "key", [], _), String (_, n) -> V_Ct (C_key (key_of_string ~raise n))
  | Prim (_, "key", [], _), Bytes (_, b) -> V_Ct (C_key (key_of_bytes ~raise b))
  | Prim (_, "bls12_381_g1", [], _), Bytes (_, b) ->
    V_Ct (C_bls12_381_g1 (bls12_381_g1_of_bytes ~raise b))
  | Prim (_, "bls12_381_g2", [], _), Bytes (_, b) ->
    V_Ct (C_bls12_381_g2 (bls12_381_g2_of_bytes ~raise b))
  | Prim (_, "bls12_381_fr", [], _), Bytes (_, b) ->
    V_Ct (C_bls12_381_fr (bls12_381_fr_of_bytes ~raise b))
  | Prim (_, "bls12_381_fr", [], _), Int (_, n) ->
    V_Ct (C_bls12_381_fr (Bls12_381_Fr.of_z n))
  | Prim (_, "signature", [], _), String (_, n) ->
    V_Ct (C_signature (signature_of_string ~raise n))
  | Prim (_, "chest", [], _), Bytes (_, b) -> V_Ct (C_chest b)
  | Prim (_, "chest_key", [], _), Bytes (_, b) -> V_Ct (C_chest_key b)
  | Prim (_, "timestamp", [], _), Int (_, n) -> V_Ct (C_timestamp n)
  | Prim (_, "timestamp", [], _), String (_, n) ->
    let open Tezos_base.TzPervasives.Time.Protocol in
    let n = Z.of_int64 (to_seconds (of_notation_exn n)) in
    V_Ct (C_timestamp n)
  | Prim (_, "mutez", [], _), Int (_, n) -> V_Ct (C_mutez n)
  | Prim (_, "bool", [], _), Prim (_, "True", [], _) -> V_Ct (C_bool true)
  | Prim (_, "bool", [], _), Prim (_, "False", [], _) -> V_Ct (C_bool false)
  | Prim (_, "string", [], _), String (_, s) -> V_Ct (C_string s)
  | Prim (_, "bytes", [], _), Bytes (_, b) -> V_Ct (C_bytes b)
  | Prim (_, "address", [], _), Bytes (_, b) ->
    V_Ct (C_address (contract_of_bytes ~raise b))
  | Prim (_, "address", [], _), String (_, s) ->
    V_Ct (C_address (contract_of_string ~raise s))
  | Prim (_, "contract", [ _ ], _), String (_, s) ->
    let address, entrypoint =
      match String.split s ~on:'%' with
      | [ a; b ] -> contract_of_string ~raise a, Some b
      | [ a ] -> contract_of_string ~raise a, None
      | _ -> raise.error (untranspilable ty value)
    in
    V_Ct (C_contract { address; entrypoint })
  | Prim (_, "unit", [], _), Prim (_, "Unit", [], _) -> V_Ct C_unit
  | Prim (_, "option", [ _ ], _), Prim (_, "None", [], _) ->
    Ligo_interpreter.Combinators.v_none ()
  | Prim (_, "option", [ o_ty ], _), Prim (_, "Some", [ s ], _) ->
    let s' = decompile_to_untyped_value ~raise ~bigmaps o_ty s in
    Ligo_interpreter.Combinators.v_some s'
  | Prim (_, "map", [ k_ty; v_ty ], _), Seq (_, lst) ->
    let lst' =
      let aux elt =
        match elt with
        | Prim (_, "Elt", [ k; v ], _) ->
          let k' = decompile_to_untyped_value ~raise ~bigmaps k_ty k in
          let v' = decompile_to_untyped_value ~raise ~bigmaps v_ty v in
          k', v'
        | _ ->
          let ty = root (strip_locations ty) in
          let value = root (strip_locations value) in
          raise.error (untranspilable ty value)
      in
      List.map ~f:aux lst
    in
    V_Map lst'
  | Prim (_, "big_map", [ k_ty; v_ty ], _), Seq (_, lst) ->
    let lst' =
      let aux elt =
        match elt with
        | Prim (_, "Elt", [ k; v ], _) ->
          let k' = decompile_to_untyped_value ~raise ~bigmaps k_ty k in
          let v' = decompile_to_untyped_value ~raise ~bigmaps v_ty v in
          k', v'
        | _ ->
          let ty = root (strip_locations ty) in
          let value = root (strip_locations value) in
          raise.error (untranspilable ty value)
      in
      List.map ~f:aux lst
    in
    V_Map lst'
  | Prim (_, "big_map", [ _; _ ], _), Int (_, v) ->
    let data : Ligo_interpreter.Types.bigmap_data =
      List.Assoc.find_exn bigmaps ~equal:Int.equal (Z.to_int v)
    in
    let lst = data.version in
    V_Map lst
  | Prim (_, "list", [ ty ], _), Seq (_, lst) ->
    let lst' =
      List.map ~f:(fun v -> decompile_to_untyped_value ~raise ~bigmaps ty v) lst
    in
    V_List lst'
  | Prim (_, "set", [ ty ], _), Seq (_, lst) ->
    let lst' =
      let aux acc cur = cur :: acc in
      let lst = List.fold_left ~f:aux ~init:lst [] in
      lst
    in
    let lst'' =
      let aux v = decompile_to_untyped_value ~raise ~bigmaps ty v in
      List.map ~f:aux lst'
    in
    V_Set lst''
  | Prim (_, "lambda", [ _; _ ], _), (Seq (_, _) as c) ->
    let open! Ast_aggregated in
    let arg_binder = Value_var.fresh ~loc () in
    (* These are temporal types, need to be patched later: *)
    let t_input = t_unit ~loc () in
    let t_output = t_unit ~loc () in
    let c = Tezos_micheline.Micheline.strip_locations c in
    let c =
      Proto_alpha_utils.Trace.trace_alpha_tzresult ~raise (fun _ ->
          Errors.generic_error Location.generated "Cannot get instructions")
      @@ Tezos_protocol.Protocol.Michelson_v1_primitives.prims_of_strings c
    in
    let u =
      Format.asprintf
        "%a"
        Tezos_micheline.Micheline_printer.print_expr
        (Tezos_micheline.Micheline_printer.printable
           Tezos_protocol.Protocol.Michelson_v1_primitives.string_of_prim
           c)
    in
    let code_block = make_e ~loc (e_string (Ligo_string.verbatim u)) (t_string ~loc ()) in
    let insertion =
      e_a_raw_code
        ~loc
        Backend.Michelson.name
        code_block
        (t_arrow ~loc t_input t_output ())
    in
    let body =
      e_a_application ~loc insertion (e_a_variable ~loc arg_binder t_input) t_output
    in
    let orig_lambda =
      e_a_lambda
        ~loc
        { binder = Param.make arg_binder t_input; output_type = t_output; result = body }
        t_input
        t_output
    in
    V_Func_val
      { rec_name = None
      ; orig_lambda
      ; arg_binder
      ; arg_mut_flag = Immutable
      ; body
      ; env = Ligo_interpreter.Environment.empty_env
      }
  | ( Prim (loct, "ticket", [ ty_ticked ], _)
    , Prim (_, "Pair", [ addr; Prim (_, "Pair", [ vt; amt ], _) ], _) ) ->
    (* note: the above Pair structure (pair addr (pair vt amt)) comes from tezos protocol *)
    let addr =
      Ligo_interpreter.Combinators.v_address
      @@
      match addr with
      | Bytes (_, b) -> contract_of_bytes ~raise b
      | String (_, s) -> contract_of_string ~raise s
      | _ -> raise.error (untranspilable ty value)
    in
    let ty_nat = Prim (loct, "nat", [], []) in
    let vt = decompile_to_untyped_value ~raise ~bigmaps ty_ticked vt in
    let amt = decompile_to_untyped_value ~raise ~bigmaps ty_nat amt in
    let va = Ligo_interpreter.Combinators.v_pair (vt, amt) in
    Ligo_interpreter.Combinators.v_pair (addr, va)
  | ty, v -> raise.error (untranspilable ty v)


(*
decompile_value [v] [t] will morph value [v] into another value using type [t].
LIGO types do not always find their equivalent in Michelson and some information can be lost:
pairs/record ; lambda lacking types ; ... (?)
e.g.
  v == (1,2,3)
  t == { a : int ; b : int ; c : int }
  result == { a = 1 ; b = 2 ; c = 3}
*)
let rec decompile_value
    ~raise
    ~(bigmaps : bigmap list)
    (v : value)
    (t : Ast_aggregated.type_expression)
    : value
  =
  let open Literal_types in
  let open Ligo_interpreter.Combinators in
  let open! Ast_aggregated in
  let self = decompile_value ~raise ~bigmaps in
  let loc = Location.interpreter in
  match t.type_content with
  | _ when Option.is_some (get_t_bool t) -> v
  | T_constant { language; injection; parameters } ->
    let () =
      Assert.assert_true
        ~raise
        (corner_case ~loc:__LOC__ ("unsupported language " ^ language))
        (String.equal language Backend.Michelson.name)
    in
    (match injection, parameters with
    | Map, [ k_ty; v_ty ] ->
      let map = trace_option ~raise (wrong_mini_c_value t v) @@ get_map v in
      let map' =
        let aux (k, v) =
          let key = self k k_ty in
          let value = self v v_ty in
          key, value
        in
        List.map ~f:aux map
      in
      V_Map map'
    | Big_map, [ k_ty; v_ty ] ->
      (match get_nat v with
      | Some _ -> raise.error @@ corner_case ~loc:"unspiller" "Big map id not supported"
      | None ->
        let big_map = trace_option ~raise (wrong_mini_c_value t v) @@ get_map v in
        let big_map' =
          let aux (k, v) =
            let key = self k k_ty in
            let value = self v v_ty in
            key, value
          in
          List.map ~f:aux big_map
        in
        V_Map big_map')
    | List, [ ty ] ->
      let lst = trace_option ~raise (wrong_mini_c_value t v) @@ get_list v in
      let lst' =
        let aux e = self e ty in
        List.map ~f:aux lst
      in
      V_List lst'
    | Set, [ ty ] ->
      let lst = trace_option ~raise (wrong_mini_c_value t v) @@ get_set v in
      let lst' =
        let aux e = self e ty in
        List.map ~f:aux lst
      in
      V_Set lst'
    | ( ( Map
        | Big_map
        | List
        | Set
        | Int64
        | String
        | Bytes
        | Int
        | Operation
        | Nat
        | Tez
        | Unit
        | Address
        | Signature
        | Key
        | Key_hash
        | Timestamp
        | Chain_id
        | Contract
        | Michelson_program
        | Michelson_or
        | Michelson_pair
        | Baker_hash
        | Pvss_key
        | Sapling_state
        | Sapling_transaction
        | Baker_operation
        | Bls12_381_g1
        | Bls12_381_g2
        | Bls12_381_fr
        | Never
        | Ticket
        | Michelson_contract
        | Gen
        | Typed_address
        | Mutation
        | External _
        | Views
        | Dynamic_entrypoint
        | Tx_rollup_l2_address
        | Chest
        | Chest_key )
      , _ ) -> v)
  | T_sum _ when Option.is_some (Ast_aggregated.get_t_bool t) -> v
  | T_sum _ when Option.is_some (Ast_aggregated.get_t_option t) ->
    let opt = trace_option ~raise (wrong_mini_c_value t v) @@ get_option v in
    (match opt with
    | None -> v_none ()
    | Some s ->
      let o = Option.value_exn (Ast_aggregated.get_t_option t) in
      let s' = self s o in
      v_some s')
  | T_sum row ->
    let Label (constructor, _), v, tv =
      Row.extract_constructor
        row
        v
        Ligo_interpreter.Combinators.get_left
        Ligo_interpreter.Combinators.get_right
    in
    let sub = self v tv in
    V_Construct (constructor, sub)
  | T_record row ->
    let lst = Row.extract_record row v Ligo_interpreter.Combinators.get_pair in
    let lst = List.map ~f:(fun (x, y, z) -> x, self y z) lst in
    let m' = Record.of_list lst in
    V_Record m'
  | T_arrow { type1; type2; param_names = _ } ->
    (* We now patch the types *)
    (* Mut flag is ignored bcs not required in the case when we patch raw code to a function *)
    let { arg_binder; arg_mut_flag = _; body; rec_name = _; orig_lambda = _; env = _ } =
      trace_option ~raise (wrong_mini_c_value t v) @@ get_func v
    in
    (match body.expression_content with
    | E_application { lamb; args = _ } ->
      (match lamb.expression_content with
      | E_raw_code { code; language = _ } ->
        let insertion =
          e_a_raw_code ~loc Backend.Michelson.name code (t_arrow ~loc type1 type2 ())
        in
        let body =
          e_a_application ~loc insertion (e_a_variable ~loc arg_binder type1) type2
        in
        let orig_lambda =
          e_a_lambda
            ~loc
            { binder = Param.make arg_binder type1; output_type = type2; result = body }
            type1
            type2
        in
        V_Func_val
          { rec_name = None
          ; orig_lambda
          ; arg_binder
          ; arg_mut_flag = Immutable
          ; body
          ; env = Ligo_interpreter.Environment.empty_env
          }
      | _ -> v)
    | _ -> v)
  | _ -> v


let conv
    ~raise
    ~bigmaps
    (t : Tezos_raw_protocol.Script_repr.expr)
    (v : Tezos_raw_protocol.Script_repr.expr)
  =
  let v =
    v
    |> Tezos_protocol.Protocol.Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
  in
  let t =
    t
    |> Tezos_protocol.Protocol.Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
  in
  decompile_to_untyped_value ~raise ~bigmaps t v
