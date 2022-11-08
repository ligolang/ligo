module Location = Simple_utils.Location
module Var = Simple_utils.Var
open Simple_utils.Trace
open Simple_utils.Option

let storage_retreival_dummy_ty = Tezos_utils.Michelson.prim "int"

let int_of_mutez t =
  Z.of_int64 @@ Memory_proto_alpha.Protocol.Alpha_context.Tez.to_mutez t


let tez_to_z : Memory_proto_alpha.Protocol.Tez_repr.t -> Z.t =
 fun t ->
  let enc = Memory_proto_alpha.Protocol.Tez_repr.encoding in
  let c = Data_encoding.Binary.to_bytes_exn enc t in
  int_of_mutez
  @@ Data_encoding.Binary.of_bytes_exn
       Memory_proto_alpha.Protocol.Alpha_context.Tez.encoding
       c


let contract_to_contract
  :  Memory_proto_alpha.Protocol.Contract_repr.t
  -> Memory_proto_alpha.Protocol.Alpha_context.Contract.t
  =
 fun t ->
  let enc = Memory_proto_alpha.Protocol.Contract_repr.encoding in
  let c = Data_encoding.Binary.to_bytes_exn enc t in
  Data_encoding.Binary.of_bytes_exn
    Memory_proto_alpha.Protocol.Alpha_context.Contract.encoding
    c


let string_of_contract t =
  Format.asprintf "%a" Memory_proto_alpha.Protocol.Alpha_context.Contract.pp t


let string_of_key_hash t =
  Format.asprintf "%a" Tezos_crypto.Signature.Public_key_hash.pp t


let string_of_key t =
  Format.asprintf "%a" Tezos_crypto.Signature.Public_key.pp t


let string_of_signature t = Format.asprintf "%a" Tezos_crypto.Signature.pp t
let bytes_of_bls12_381_g1 t = Bls12_381.G1.to_bytes t
let bytes_of_bls12_381_g2 t = Bls12_381.G2.to_bytes t
let bytes_of_bls12_381_fr t = Bls12_381.Fr.to_bytes t

module Tezos_eq = struct
  (* behavior should be equivalent to the one in the tezos codebase *)
  let nat_shift_left x y =
    if Z.compare y (Z.of_int 256) > 0
    then None
    else (
      let y = Z.to_int y in
      Some (Z.shift_left x y))


  let nat_shift_right x y =
    if Z.compare y (Z.of_int 256) > 0
    then None
    else (
      let y = Z.to_int y in
      Some (Z.shift_right x y))


  let int_ediv x y =
    try
      let q, r = Z.ediv_rem x y in
      Some (q, r)
    with
    | _ -> None


  let timestamp_add : Z.t -> Z.t -> Z.t =
   fun tz n ->
    let open Memory_proto_alpha.Protocol.Script_timestamp in
    let t = of_zint tz in
    add_delta t (Memory_proto_alpha.Protocol.Script_int.of_zint n) |> to_zint


  let timestamp_sub : Z.t -> Z.t -> Z.t =
   fun tz n ->
    let open Memory_proto_alpha.Protocol.Script_timestamp in
    let t = of_zint tz in
    sub_delta t (Memory_proto_alpha.Protocol.Script_int.of_zint n) |> to_zint


  let mutez_add : Z.t -> Z.t -> Z.t option =
   fun x y ->
    let open Memory_proto_alpha.Protocol.Alpha_context.Tez in
    let open Option in
    try
      let x = Z.to_int64 x in
      let y = Z.to_int64 y in
      let* x = of_mutez x in
      let* y = of_mutez y in
      match x +? y with
      | Ok t -> some @@ Z.of_int64 (to_mutez t)
      | _ -> None
    with
    | Z.Overflow -> None


  let mutez_sub : Z.t -> Z.t -> Z.t option =
   fun x y ->
    let open Memory_proto_alpha.Protocol.Alpha_context.Tez in
    let open Option in
    try
      let x = Z.to_int64 x in
      let y = Z.to_int64 y in
      let* x = of_mutez x in
      let* y = of_mutez y in
      match x -? y with
      | Ok t -> some @@ Z.of_int64 (to_mutez t)
      | _ -> None
    with
    | Z.Overflow -> None
end

let create_chest_key (chest : bytes) (time : int) : bytes =
  let open Tezos_crypto in
  let chest = Data_encoding.Binary.of_bytes_exn Timelock.chest_encoding chest in
  Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding
  @@ Timelock.create_chest_key chest ~time


let create_chest (payload : Bytes.t) (time : int) : _ =
  let open Tezos_crypto in
  let chest, chest_key = Timelock.create_chest_and_chest_key ~payload ~time in
  let chest_key_bytes =
    Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding chest_key
  in
  let chest_bytes =
    Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding chest
  in
  chest_bytes, chest_key_bytes


let clean_location_with v x =
  let open Tezos_micheline.Micheline in
  inject_locations (fun _ -> v) (strip_locations x)


let clean_locations e t = clean_location_with () e, clean_location_with () t

let add_ast_env ?(name = Ligo_prim.Value_var.fresh ()) env binder body =
  let open Ast_aggregated in
  let aux (let_binder, expr, no_mutation, inline) (e : expression) =
    if Ligo_prim.Value_var.compare let_binder binder <> 0
       && Ligo_prim.Value_var.compare let_binder name <> 0
    then
      e_a_let_in
        (Ligo_prim.Binder.make let_binder expr.type_expression)
        expr
        e
        { inline
        ; no_mutation
        ; view = false
        ; public = false
        ; hidden = false
        ; thunk = false
        }
    else e
  in
  let typed_exp' = List.fold_right ~f:aux ~init:body env in
  typed_exp'


let make_options ~raise ?param ctxt =
  let open Ligo_run.Of_michelson in
  let default =
    { now = None
    ; amount = ""
    ; balance = ""
    ; sender = None
    ; source = None
    ; parameter_ty = param
    }
  in
  match ctxt with
  | None -> make_dry_run_options ~raise default
  | Some (ctxt : Tezos_state.context) ->
    let source = ctxt.internals.source in
    let tezos_context = Tezos_state.get_alpha_context ~raise ctxt in
    let tezos_context =
      Memory_proto_alpha.Protocol.Alpha_context.Gas.set_limit
        tezos_context
        (Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.integral_exn
           (Z.of_int 800000))
    in
    let timestamp =
      Memory_proto_alpha.Protocol.Script_timestamp.of_zint
        (Z.of_int64
           (Proto_alpha_utils.Time.Protocol.to_seconds
              (Tezos_state.get_timestamp ctxt)))
    in
    let level =
      Memory_proto_alpha.Protocol.(
        (Alpha_context.Level.current tezos_context).level
        |> Alpha_context.Raw_level.to_int32
        |> Script_int.of_int32
        |> Script_int.abs)
    in
    { tezos_context
    ; source
    ; payer = source
    ; self = source
    ; amount =
        Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez_exn 100000000L
    ; chain_id = Memory_proto_alpha.Alpha_environment.Chain_id.zero
    ; balance = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero
    ; now = timestamp
    ; level
    }


let run_expression_unwrap
  ~raise
  ~run_options
  ?(loc = Location.generated)
  (c_expr : Stacking.compiled_expression)
  =
  let runres =
    Ligo_run.Of_michelson.run_expression
      ~raise
      ~options:run_options
      c_expr.expr
      c_expr.expr_ty
  in
  match runres with
  | Success (expr_ty, expr) ->
    let expr, expr_ty = clean_locations expr expr_ty in
    expr, expr_ty
  | Fail _ -> raise.error @@ Errors.generic_error loc "Running failed"


let compile_ast ~raise ~options aggregated_exp =
  let open Ligo_compile in
  let mini_c_exp = Of_aggregated.compile_expression ~raise aggregated_exp in
  Of_mini_c.compile_expression ~raise ~options mini_c_exp


let compile_type ~raise type_exp =
  let open Ligo_compile in
  let ty = Of_aggregated.compile_type ~raise type_exp in
  Of_mini_c.compile_type ty


let entrypoint_of_string x =
  match
    Memory_proto_alpha.Raw_protocol.Entrypoint_repr.of_annot_lax_opt
      (Memory_proto_alpha.Raw_protocol.Non_empty_string.of_string_exn x)
  with
  | Some x -> x
  | None ->
    failwith (Format.asprintf "Testing framework: Invalid entrypoint %s" x)


let build_ast ~raise subst_lst arg_binder rec_name in_ty out_ty aggregated_exp =
  let aggregated_exp' = add_ast_env subst_lst arg_binder aggregated_exp in
  let aggregated_exp =
    match rec_name with
    | None ->
      Ast_aggregated.e_a_lambda
        { result = aggregated_exp'
        ; output_type = out_ty
        ; binder = Ligo_prim.Param.make arg_binder in_ty
        }
        in_ty
        out_ty
    | Some fun_name ->
      Ast_aggregated.e_a_recursive
        { fun_name
        ; fun_type = Ast_aggregated.t_arrow in_ty out_ty ()
        ; lambda =
            { result = aggregated_exp'
            ; output_type = out_ty
            ; binder = Ligo_prim.Param.make arg_binder in_ty
            }
        }
  in
  let parameter, storage =
    trace_option
      ~raise
      (Errors.generic_error
         Location.generated
         "Trying to compile a non-contract?")
    @@ Ast_aggregated.get_t_pair in_ty
  in
  trace ~raise Main_errors.self_ast_aggregated_tracer
  @@ Self_ast_aggregated.all_contract parameter storage aggregated_exp


let compile_contract_ast ~raise ~options ~tezos_context main views =
  let open Ligo_compile in
  let mini_c = Of_aggregated.compile_expression ~raise main in
  let main_michelson = Of_mini_c.compile_contract ~raise ~options mini_c in
  let views =
    match views with
    | None -> []
    | Some (view_names, aggregated) ->
      let mini_c =
        Ligo_compile.Of_aggregated.compile_expression ~raise aggregated
      in
      let mini_c =
        trace ~raise Main_errors.self_mini_c_tracer
        @@ Self_mini_c.all_expression options mini_c
      in
      let mini_c_tys =
        trace_option
          ~raise
          (`Self_mini_c_tracer
            (Self_mini_c.Errors.corner_case
               "Error reconstructing type of views"))
        @@ Mini_c.get_t_tuple mini_c.type_expression
      in
      let nb_of_views = List.length view_names in
      let aux i view =
        let idx_ty =
          trace_option
            ~raise
            (`Self_mini_c_tracer
              (Self_mini_c.Errors.corner_case
                 "Error reconstructing type of view"))
          @@ List.nth mini_c_tys i
        in
        let idx = Mini_c.e_proj mini_c idx_ty i nb_of_views in
        view, idx
      in
      let views = List.mapi ~f:aux view_names in
      let aux (vn, mini_c) =
        vn, Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c
      in
      let michelsons = List.map ~f:aux views in
      let () =
        Ligo_compile.Of_michelson.check_view_restrictions
          ~raise
          (List.map ~f:snd michelsons)
      in
      michelsons
  in
  let contract =
    Ligo_compile.Of_michelson.build_contract
      ~raise
      ~has_env_comments:false
      ~protocol_version:options.middle_end.protocol_version
      ~disable_typecheck:false
      ~tezos_context
      main_michelson
      views
  in
  Tezos_utils.Micheline.Micheline.map_node (fun _ -> ()) (fun x -> x) contract


let compile_contract_file ~raise ~options source_file entry_point declared_views
  =
  let aggregated, views =
    Build.build_contract_meta_ligo
      ~raise
      ~options
      entry_point
      declared_views
      source_file
  in
  aggregated, views


let make_function in_ty out_ty arg_binder body subst_lst =
  let typed_exp' = add_ast_env subst_lst arg_binder body in
  Ast_aggregated.e_a_lambda
    { result = typed_exp'
    ; output_type = out_ty
    ; binder = Ligo_prim.Param.make arg_binder in_ty
    }
    in_ty
    out_ty


let rec val_to_ast ~raise ~loc
  : Ligo_interpreter.Types.value -> Ast_aggregated.type_expression -> _
  =
 fun v ty ->
  let open Ligo_interpreter.Types in
  let open Ast_aggregated in
  match v with
  | V_Ct C_unit ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected unit but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_unit ty)
    in
    e_a_unit ()
  | V_Ct (C_bool b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bool but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bool ty)
    in
    e_a_bool b
  | V_Ct (C_int x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected int but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (match ty.type_content with
         | T_constant { injection = Int | External "int"; _ } -> Some ()
         | _ -> None)
    in
    e_a_int x
  | V_Ct (C_int64 _) ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: int64"
  | V_Ct (C_nat x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected nat but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_nat ty)
    in
    e_a_nat x
  | V_Ct (C_mutez x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected mutez but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_mutez ty)
    in
    e_a_mutez x
  | V_Ct (C_timestamp t) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected timestamp but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_timestamp ty)
    in
    e_a_timestamp t
  | V_Ct (C_string s) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected string but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_string ty)
    in
    e_a_string (Simple_utils.Ligo_string.standard s)
  | V_Ct (C_bytes b) ->
    (match get_t_bytes ty with
     | Some () -> e_a_bytes b
     | None ->
       (match get_t_chest ty with
        | Some () -> e_a_chest b
        | None ->
          (match get_t_chest_key ty with
           | Some () -> e_a_chest_key b
           | None ->
             raise.error
               (Errors.generic_error
                  loc
                  (Format.asprintf
                     "Expected bytes, chest, or chest_key but got %a"
                     Ast_aggregated.PP.type_expression
                     ty)))))
  | V_Ct (C_address a) when is_t_address ty ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected address or typed address but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_address ty)
    in
    let x = string_of_contract a in
    e_a_address x
  | V_Ct (C_address _) ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected address or typed address but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_Ct (C_contract _) when is_t_contract ty ->
    raise.error (Errors.generic_error loc "Not implemented: contract to ast")
  | V_Ct (C_contract _) ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected contract but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_Ct (C_key_hash kh) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected key_hash but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_key_hash ty)
    in
    let x = string_of_key_hash kh in
    e_a_key_hash x
  | V_Ct (C_key k) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected key but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_key ty)
    in
    let x = string_of_key k in
    e_a_key x
  | V_Ct (C_signature s) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected signature but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_signature ty)
    in
    let x = string_of_signature s in
    e_a_signature x
  | V_Ct (C_bls12_381_g1 b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bls12_381_g1 but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bls12_381_g1 ty)
    in
    let x = bytes_of_bls12_381_g1 b in
    e_a_bls12_381_g1 x
  | V_Ct (C_bls12_381_g2 b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bls12_381_g2 but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bls12_381_g2 ty)
    in
    let x = bytes_of_bls12_381_g2 b in
    e_a_bls12_381_g2 x
  | V_Ct (C_bls12_381_fr b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bls12_381_fr but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bls12_381_fr ty)
    in
    let x = bytes_of_bls12_381_fr b in
    e_a_bls12_381_fr x
  | V_Ct (C_chain_id s) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected chain_id but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (match ty.type_content with
         | T_constant { injection = Chain_id; _ } -> Some ()
         | _ -> None)
    in
    e_a_chain_id s
  | V_Construct (ctor, arg) when is_t_sum ty ->
    let map_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected sum type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_sum_opt ty
    in
    let ({ associated_type = ty'; michelson_annotation = _; decl_pos = _ }
          : row_element)
      =
      Ligo_prim.Record.LMap.find (Label ctor) map_ty.fields
    in
    let arg = val_to_ast ~raise ~loc arg ty' in
    e_a_constructor ctor arg ty
  | V_Construct _ ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected sum type but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_Func_val v ->
    make_ast_func
      ~raise
      ?name:v.rec_name
      v.env
      v.arg_binder
      v.body
      v.orig_lambda
  | V_Michelson (Ty_code { micheline_repr = { code; code_ty = _ }; ast_ty }) ->
    let s = Format.asprintf "%a" Tezos_utils.Michelson.pp code in
    let s = Ligo_string.verbatim s in
    e_a_raw_code Backend.Michelson.name (make_e (e_string s) ast_ty) ast_ty
  | V_Record map when is_t_record ty ->
    let map_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected record type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_record_opt ty
    in
    make_ast_record ~raise ~loc map_ty map
  | V_Record map when Option.is_some @@ get_t_ticket ty ->
    let ty =
      trace_option ~raise (Errors.generic_error loc "impossible")
      @@ get_t_ticket ty
    in
    let rows =
      trace_option ~raise (Errors.generic_error loc "impossible")
      @@ get_t_record (Ast_aggregated.t_unforged_ticket ty)
    in
    let map =
      let get l map =
        trace_option
          ~raise
          (Errors.generic_error loc "bad unforged ticket")
          (Ligo_prim.Record.LMap.find_opt l map)
      in
      (*  at this point the record value is a nested pair (extracted from michelson), e.g. (KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL , (0x05010000000474657374 , 10n)) *)
      let ticketer = get (Label "0") map in
      let map =
        match get (Label "1") map with
        | V_Record map -> map
        | _ ->
          raise.error
          @@ Errors.generic_error loc "unforged ticket badly decompiled"
      in
      let value = get (Label "0") map in
      let amt = get (Label "1") map in
      Ligo_prim.Record.of_list
        [ Label "ticketer", ticketer
        ; Label "value", value
        ; Label "amount", amt
        ]
    in
    make_ast_record ~raise ~loc rows map
  | V_Record _ ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected record type but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_List l ->
    let ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected list but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_list ty
    in
    make_ast_list ~raise ~loc ty l
  | V_Set l ->
    let ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected set but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_set ty
    in
    make_ast_set ~raise ~loc ty l
  | V_Map kv when is_t_big_map ty ->
    let key_ty, value_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected big_map but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_big_map ty
    in
    make_ast_big_map ~raise ~loc key_ty value_ty kv
  | V_Map kv when is_t_map ty ->
    let key_ty, value_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected map but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_map ty
    in
    make_ast_map ~raise ~loc key_ty value_ty kv
  | V_Map _ ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected map or big_map but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_Michelson_contract _ ->
    raise.error
    @@ Errors.generic_error loc "Cannot be abstracted: michelson-contract"
  | V_Ast_contract _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: ast-contract"
  | V_Michelson (Untyped_code _) ->
    raise.error
    @@ Errors.generic_error loc "Cannot be abstracted: untyped-michelson-code"
  | V_Mutation _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: mutation"
  | V_Gen _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: generator"
  | V_Location _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: location"
  | V_Typed_address _ ->
    raise.error
    @@ Errors.generic_error loc "Cannot be abstracted: typed_address"


and make_ast_func ~raise ?name env arg body orig =
  let open Ast_aggregated in
  let env = make_subst_ast_env_exp ~raise env in
  let typed_exp' = add_ast_env ?name env arg body in
  let Ligo_prim.Arrow.{ type1 = in_ty; type2 = out_ty } =
    get_t_arrow_exn orig.type_expression
  in
  let lambda =
    Ligo_prim.Lambda.
      { result = typed_exp'
      ; output_type = out_ty
      ; binder = Ligo_prim.Param.make arg in_ty
      }
  in
  let typed_exp' =
    match name with
    | None -> e_a_lambda lambda in_ty out_ty
    | Some fun_name ->
      e_a_recursive { fun_name; fun_type = orig.type_expression; lambda }
  in
  (* Check that function to be compiled is obj-LIGO *)
  let _ =
    trace ~raise Main_errors.self_ast_aggregated_tracer
    @@ Self_ast_aggregated.expression_obj typed_exp'
  in
  typed_exp'


and make_ast_record ~raise ~loc (map_ty : Ast_aggregated.t_sum) map =
  let open Ligo_interpreter.Types in
  let kv_list =
    Ast_aggregated.Helpers.kv_list_of_t_record_or_tuple
      ~layout:map_ty.layout
      map_ty.fields
  in
  let kv_list =
    List.map
      ~f:(fun (l, ty) ->
        let value = Ligo_prim.Record.LMap.find l map in
        let ast = val_to_ast ~raise ~loc value ty.associated_type in
        l, ast)
      kv_list
  in
  Ast_aggregated.ez_e_a_record ~layout:map_ty.layout kv_list


and make_ast_list ~raise ~loc ty l =
  let l = List.map ~f:(fun v -> val_to_ast ~raise ~loc v ty) l in
  List.fold_right l ~f:Ast_aggregated.e_a_cons ~init:(Ast_aggregated.e_a_nil ty)


and make_ast_set ~raise ~loc ty l =
  let l =
    List.dedup_and_sort ~compare:Ligo_interpreter.Combinators.compare_value l
  in
  let l = List.map ~f:(fun v -> val_to_ast ~raise ~loc v ty) l in
  List.fold_right
    l
    ~f:Ast_aggregated.e_a_set_add
    ~init:(Ast_aggregated.e_a_set_empty ty)


and make_ast_big_map ~raise ~loc key_ty value_ty kv =
  let kv =
    List.dedup_and_sort
      ~compare:(fun (k, _) (k', _) ->
        Ligo_interpreter.Combinators.compare_value k k')
      kv
  in
  let kv =
    List.map
      ~f:(fun (k, v) ->
        let k = val_to_ast ~raise ~loc k key_ty in
        let v = val_to_ast ~raise ~loc v value_ty in
        k, v)
      kv
  in
  List.fold_right
    kv
    ~f:(fun (k, v) r -> Ast_aggregated.e_a_big_map_add k v r)
    ~init:(Ast_aggregated.e_a_big_map_empty key_ty value_ty)


and make_ast_map ~raise ~loc key_ty value_ty kv =
  let kv =
    List.dedup_and_sort
      ~compare:(fun (k, _) (k', _) ->
        Ligo_interpreter.Combinators.compare_value k k')
      kv
  in
  let kv =
    List.map
      ~f:(fun (k, v) ->
        let k = val_to_ast ~raise ~loc k key_ty in
        let v = val_to_ast ~raise ~loc v value_ty in
        k, v)
      kv
  in
  List.fold_right
    kv
    ~f:(fun (k, v) r -> Ast_aggregated.e_a_map_add k v r)
    ~init:(Ast_aggregated.e_a_map_empty key_ty value_ty)


and make_subst_ast_env_exp ~raise env =
  let open Ligo_interpreter.Types in
  let rec aux acc = function
    | [] -> acc
    | (name, { item; no_mutation; inline }) :: tl ->
      let expr =
        val_to_ast
          ~raise
          ~loc:(Ligo_prim.Value_var.get_location name)
          item.eval_term
          item.ast_type
      in
      aux ((name, expr, no_mutation, inline) :: acc) tl
  in
  aux [] env


let rec compile_value ~raise ~options ~loc
  :  Ligo_interpreter.Types.value -> Ast_aggregated.type_expression
  -> Ligo_interpreter.Types.mcode
  =
 fun v ty ->
  let open Ast_aggregated in
  let open Ast_aggregated.Combinators in
  let self = compile_value ~raise ~options ~loc in
  match v with
  | V_Ct (C_string s) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected string but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_string ty)
    in
    Tezos_micheline.Micheline.String ((), s)
  | V_Ct (C_bytes b) ->
    (match get_t_bytes ty with
     | Some () -> Tezos_micheline.Micheline.Bytes ((), b)
     | None ->
       (match get_t_chest ty with
        | Some () -> Tezos_micheline.Micheline.Bytes ((), b)
        | None ->
          (match get_t_chest_key ty with
           | Some () -> Tezos_micheline.Micheline.Bytes ((), b)
           | None ->
             raise.error
               (Errors.generic_error
                  loc
                  (Format.asprintf
                     "Expected bytes, chest, or chest_key but got %a"
                     Ast_aggregated.PP.type_expression
                     ty)))))
  | V_Ct (C_int x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected int but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_int ty)
    in
    Tezos_micheline.Micheline.Int ((), x)
  | V_Ct (C_nat x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected nat but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_nat ty)
    in
    Tezos_micheline.Micheline.Int ((), x)
  | V_Ct (C_mutez x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected mutez but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_mutez ty)
    in
    Tezos_micheline.Micheline.Int ((), x)
  | V_Ct C_unit ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected unit but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_unit ty)
    in
    Tezos_micheline.Micheline.Prim ((), "Unit", [], [])
  | V_Ct (C_bool true) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bool but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bool ty)
    in
    Tezos_micheline.Micheline.Prim ((), "True", [], [])
  | V_Ct (C_bool false) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bool but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bool ty)
    in
    Tezos_micheline.Micheline.Prim ((), "False", [], [])
  | V_Ct (C_address a) when is_t_address ty ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected address but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_address ty)
    in
    let x = string_of_contract a in
    Tezos_micheline.Micheline.String ((), x)
  | V_Ct (C_address _) ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected address or typed address but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_Ct (C_contract c) when is_t_contract ty ->
    let x = string_of_contract c.address in
    (* TODO-er: if we want support for entrypoints, this should be fixed: *)
    (match c.entrypoint with
     | None -> Tezos_micheline.Micheline.String ((), x)
     | Some e -> Tezos_micheline.Micheline.String ((), x ^ "%" ^ e))
  | V_Ct (C_key_hash kh) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected key_hash but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_key_hash ty)
    in
    let x = string_of_key_hash kh in
    Tezos_micheline.Micheline.String ((), x)
  | V_Ct (C_key k) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected key but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_key ty)
    in
    let x = string_of_key k in
    Tezos_micheline.Micheline.String ((), x)
  | V_Ct (C_signature s) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected signature but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_signature ty)
    in
    let x = string_of_signature s in
    Tezos_micheline.Micheline.String ((), x)
  | V_Ct (C_chain_id s) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected chain_id but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (match ty.type_content with
         | T_constant { injection = Chain_id; _ } -> Some ()
         | _ -> None)
    in
    Tezos_micheline.Micheline.String ((), s)
  | V_Ct (C_bls12_381_g1 b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bls12_381_g1 but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bls12_381_g1 ty)
    in
    let x = bytes_of_bls12_381_g1 b in
    Tezos_micheline.Micheline.Bytes ((), x)
  | V_Ct (C_bls12_381_g2 b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bls12_381_g2 but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bls12_381_g2 ty)
    in
    let x = bytes_of_bls12_381_g2 b in
    Tezos_micheline.Micheline.Bytes ((), x)
  | V_Ct (C_bls12_381_fr b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bls12_381_fr but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_bls12_381_fr ty)
    in
    let x = bytes_of_bls12_381_fr b in
    Tezos_micheline.Micheline.Bytes ((), x)
  | V_Ct (C_timestamp t) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
            loc
            (Format.asprintf
              "Expected timestamp but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_timestamp ty)
    in
    Tezos_micheline.Micheline.Int ((),t)
  | V_Ct (C_int64 x) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
            loc
            (Format.asprintf
              "Expected timestamp but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_int ty)
    in
    Tezos_micheline.Micheline.Int ((),Z.of_int64 x)
  | V_Construct (ctor, arg) when Option.is_some (get_t_option ty) ->
    (match ctor with
     | "None" -> Tezos_micheline.Micheline.Prim ((), "None", [], [])
     | "Some" ->
       let option_ty =
         trace_option
           ~raise
           (Errors.generic_error
              loc
              (Format.asprintf
                 "Expected option type but got %a"
                 Ast_aggregated.PP.type_expression
                 ty))
         @@ get_t_option ty
       in
       let arg = self arg option_ty in
       Tezos_micheline.Micheline.Prim ((), "Some", [ arg ], [])
     | _ -> failwith "Unexpected")
  | V_Construct (ctor, arg) when is_t_sum ty ->
    let map_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected sum type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_sum_opt ty
    in
    let ({ associated_type = ty'; michelson_annotation = _; decl_pos = _ }
          : row_element)
      =
      Ligo_prim.Record.LMap.find (Label ctor) map_ty.fields
    in
    let arg = self arg ty' in
    let ty' = Ligo_compile.Of_aggregated.compile_type ~raise ty in
    let ty_variant =
      trace_option ~raise (Errors.generic_error Location.generated "foo")
      @@ get_t_sum_opt ty
    in
    let path =
      Layout.constructor_to_lr
        ~raise
        ~layout:ty_variant.layout
        ty'
        ty_variant.fields
        (Ligo_prim.Label.Label ctor)
    in
    let aux pred (_ty, lr) =
      match lr with
      | `Left -> Tezos_micheline.Micheline.Prim ((), "Left", [ pred ], [])
      | `Right -> Tezos_micheline.Micheline.Prim ((), "Right", [ pred ], [])
    in
    List.fold ~f:aux ~init:arg path
  | V_Construct _ ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Expected sum type but got %a"
            Ast_aggregated.PP.type_expression
            ty)
  | V_Record map when is_t_record ty ->
    let map_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected record type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_record_opt ty
    in
    let map_kv =
      Ligo_prim.Record.LMap.mapi
        (fun l v ->
          let ({ associated_type; _ } : row_element) =
            Ligo_prim.Record.LMap.find l map_ty.fields
          in
          associated_type, v)
        map
    in
    Layout.record_to_pairs
      ~raise
      (fun (t, v) -> self v t)
      (fun e1 e2 -> Tezos_micheline.Micheline.Prim ((), "Pair", [ e1; e2 ], []))
      (fun es -> Tezos_micheline.Micheline.Prim ((), "Pair", es, []))
      map_ty
      map_kv
  | V_List lst ->
    let lst_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected list type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_list ty
    in
    let lst = List.map ~f:(fun v -> self v lst_ty) lst in
    Tezos_micheline.Micheline.Seq ((), lst)
  | V_Set lst ->
    let lst_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected set type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_set ty
    in
    let lst = List.map ~f:(fun v -> self v lst_ty) lst in
    Tezos_micheline.Micheline.Seq ((), lst)
  | V_Map map when is_t_map ty ->
    let k_ty, v_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected map type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_map ty
    in
    let map =
      List.map
        ~f:(fun (k, v) ->
          let k = self k k_ty in
          let v = self v v_ty in
          k, v)
        map
    in
    let map =
      List.sort ~compare:(fun (k1, _) (k2, _) -> Caml.compare k1 k2) map
    in
    let map =
      List.map
        ~f:(fun (k, v) ->
          Tezos_micheline.Micheline.Prim ((), "Elt", [ k; v ], []))
        map
    in
    Tezos_micheline.Micheline.Seq ((), map)
  | V_Map map when is_t_big_map ty ->
    let k_ty, v_ty =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected map type but got %a"
              Ast_aggregated.PP.type_expression
              ty))
      @@ get_t_big_map ty
    in
    let map =
      List.map
        ~f:(fun (k, v) ->
          let k = self k k_ty in
          let v = self v v_ty in
          k, v)
        map
    in
    let map =
      List.sort ~compare:(fun (k1, _) (k2, _) -> Caml.compare k1 k2) map
    in
    let map =
      List.map
        ~f:(fun (k, v) ->
          Tezos_micheline.Micheline.Prim ((), "Elt", [ k; v ], []))
        map
    in
    Tezos_micheline.Micheline.Seq ((), map)
  | V_Func_val v -> (
    let make_subst_ast_env_exp ~raise env =
      let open Ligo_interpreter.Types in
      let rec aux acc = function
        | [] -> acc
        | (name, { item; no_mutation; inline }) :: tl ->
          let mich = self item.eval_term item.ast_type in
          let minic_ty =
            Ligo_compile.Of_aggregated.compile_type ~raise item.ast_type
          in
          let mich_ty = Ligo_compile.Of_mini_c.compile_type minic_ty in
          let mich_ty =
            Tezos_micheline.(
              Micheline.map_node (fun _ -> ()) (fun x -> x) mich_ty)
          in
          let mich =
            Tezos_micheline.Micheline.(
              Seq
                ( ()
                , [ Prim ((), "DROP", [], [])
                  ; Prim ((), "PUSH", [ mich_ty; mich ], [])
                  ] ))
          in
          let mich =
            Tezos_micheline.(
              Micheline.map_node
                (fun _ -> Micheline_printer.{ comment = None })
                (fun x -> x)
                mich)
          in
          let mich =
            Format.asprintf
              "%a"
              Tezos_micheline.Micheline_printer.print_expr
              mich
          in
          let expr =
            e_a_raw_code
              "Michelson"
              (make_e
                 (e_string @@ Ligo_string.verbatim mich)
                 (t_arrow (t_unit ()) item.ast_type ()))
              (t_arrow (t_unit ()) item.ast_type ())
          in
          let expr = e_a_application expr (e_a_unit ()) item.ast_type in
          aux ((name, expr, no_mutation, inline) :: acc) tl
      in
      aux [] env
    in
    let make_ast_func ~raise ?name env arg body orig =
      let open Ast_aggregated in
      let env = make_subst_ast_env_exp ~raise env in
      let typed_exp' = add_ast_env ?name env arg body in
      let Ligo_prim.Arrow.{ type1 = in_ty; type2 = out_ty } =
        get_t_arrow_exn orig.type_expression
      in
      let lambda =
        Ligo_prim.Lambda.
          { result = typed_exp'
          ; output_type = out_ty
          ; binder = Ligo_prim.Param.make arg in_ty
          }
      in
      let typed_exp' =
        match name with
        | None -> e_a_lambda lambda in_ty out_ty
        | Some fun_name ->
          e_a_recursive { fun_name; fun_type = orig.type_expression; lambda }
      in
      (* Check that function to be compiled is obj-LIGO *)
      let _ =
        trace ~raise Main_errors.self_ast_aggregated_tracer
        @@ Self_ast_aggregated.expression_obj typed_exp'
      in
      typed_exp'
    in
    let typed_exp =
      make_ast_func
        ~raise
        ?name:v.rec_name
        v.env
        v.arg_binder
        v.body
        v.orig_lambda
    in
    let compiled_exp = compile_ast ~raise ~options typed_exp in
    match compiled_exp.expr with
     | Seq (_, [ Prim (_, "LAMBDA", [ _; _; compiled_exp ], _) ]) ->
       let compiled_exp =
         Tezos_micheline.Micheline.map_node
           (fun _ -> ())
           (fun x -> x)
           compiled_exp
       in
       compiled_exp
     | _ ->
       raise.error
       @@ Errors.generic_error loc (Format.asprintf "Expected LAMBDA"))
  | v ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Cannot decompile value %a of type %a"
            Ligo_interpreter.PP.pp_value v
            Ast_aggregated.PP.type_expression ty)


let compile_value ~raise ~options ~loc
  :  Ligo_interpreter.Types.value -> Ast_aggregated.type_expression
  -> Ligo_interpreter.Types.typed_michelson_code
  =
 fun v ty ->
  let expr = compile_value ~raise ~options ~loc v ty in
  let expr_ty = Ligo_compile.Of_aggregated.compile_type ~raise ty in
  let expr_ty = Ligo_compile.Of_mini_c.compile_type expr_ty in
  let expr_ty = clean_location_with () expr_ty in
  Ligo_interpreter.Types.
    { micheline_repr = { code = expr; code_ty = expr_ty }; ast_ty = ty }


let run_michelson_func
  ~raise
  ~options
  ~loc
  (ctxt : Tezos_state.context)
  (code : (unit, string) Tezos_micheline.Micheline.node)
  func_ty
  arg
  arg_ty
  =
  let open Ligo_interpreter.Types in
  let run_options = make_options ~raise (Some ctxt) in
  let { micheline_repr = { code = arg; code_ty = arg_ty }; _ } =
    compile_value ~raise ~options ~loc arg arg_ty
  in
  let func_ty = compile_type ~raise func_ty in
  let func =
    match code with
    | Seq (_, s) -> Tezos_utils.Michelson.(seq ([ i_push arg_ty arg ] @ s))
    | _ ->
      raise.error (Errors.generic_error Location.generated "Could not parse")
  in
  match
    Ligo_run.Of_michelson.run_expression
      ~raise
      ~legacy:true
      ~options:run_options
      func
      func_ty
  with
  | Success (ty, value) ->
    Result.return
    @@ Michelson_to_value.decompile_to_untyped_value
         ~raise
         ~bigmaps:ctxt.transduced.bigmaps
         ty
         value
  | Fail f -> Result.fail f


let parse_code ~raise code =
  let open Tezos_micheline in
  let code, errs = Micheline_parser.tokenize code in
  let code =
    match errs with
    | _ :: _ ->
      raise.error (Errors.generic_error Location.generated "Could not parse")
    | [] ->
      let code, errs = Micheline_parser.parse_expression ~check:false code in
      (match errs with
       | _ :: _ ->
         raise.error (Errors.generic_error Location.generated "Could not parse")
       | [] ->
         let code = Micheline.map_node (fun _ -> ()) (fun x -> x) code in
         (match code with
          | Seq (_, s) -> Tezos_utils.Michelson.(seq s)
          | _ ->
            raise.error
              (Errors.generic_error Location.generated "Could not parse")))
  in
  code


let parse_and_run_michelson_func
  ~raise
  ~loc
  (ctxt : Tezos_state.context)
  code
  func_ty
  arg
  arg_ty
  =
  let code = parse_code ~raise code in
  run_michelson_func ~raise ~loc ctxt code func_ty arg arg_ty


let parse_raw_michelson_code ~raise code ty =
  let open Tezos_micheline in
  let ty = compile_type ~raise ty in
  let code = parse_code ~raise code in
  let code_ty = Micheline.map_node (fun _ -> ()) (fun x -> x) ty in
  code, code_ty
