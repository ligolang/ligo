module Location = Simple_utils.Location
module Var = Simple_utils.Var
open Simple_utils.Trace
open Simple_utils.Option

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


let string_of_key t = Format.asprintf "%a" Tezos_crypto.Signature.Public_key.pp t
let string_of_signature t = Format.asprintf "%a" Tezos_crypto.Signature.pp t
let bytes_of_bls12_381_g1 t = Bls12_381.G1.to_bytes t
let bytes_of_bls12_381_g2 t = Bls12_381.G2.to_bytes t
let bytes_of_bls12_381_fr t = Bls12_381.Fr.to_bytes t
let string_of_chain_id t = Tezos_crypto.Hashed.Chain_id.to_b58check t

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
  let chest, chest_key = Timelock.create_chest_and_chest_key ~payload ~time () in
  let chest_key_bytes =
    Data_encoding.Binary.to_bytes_exn Timelock.chest_key_encoding chest_key
  in
  let chest_bytes = Data_encoding.Binary.to_bytes_exn Timelock.chest_encoding chest in
  chest_bytes, chest_key_bytes


let verify_chest (chest : bytes) (chest_key : bytes) (time : int) : _ =
  let open Tezos_crypto in
  let chest = Data_encoding.Binary.of_bytes_exn Timelock.chest_encoding chest in
  let chest_key =
    Data_encoding.Binary.of_bytes_exn Timelock.chest_key_encoding chest_key
  in
  Timelock.verify ~time chest.locked_value chest_key


let clean_location_with v x =
  let open Tezos_micheline.Micheline in
  inject_locations (fun _ -> v) (strip_locations x)


let clean_locations e t = clean_location_with () e, clean_location_with () t

let add_ast_env
    ?(name = Ligo_prim.Value_var.fresh ~loc:Location.interpreter ())
    env
    binder
    body
  =
  let open Ast_aggregated in
  let loc = body.location in
  let aux (let_binder, expr, no_mutation, inline) (e : expression) =
    if Ligo_prim.Value_var.compare let_binder binder <> 0
       && Ligo_prim.Value_var.compare let_binder name <> 0
    then
      e_a_let_in
        ~loc
        (Pattern.var ~loc (Ligo_prim.Binder.make let_binder expr.type_expression))
        expr
        e
        { ValueAttr.default_attributes with inline; no_mutation; public = false }
    else e
  in
  let typed_exp' = List.fold_right ~f:aux ~init:body env in
  typed_exp'


let make_options ~raise ?param ctxt =
  let open Lwt.Let_syntax in
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
    let%map tezos_context = Tezos_state.get_alpha_context ~raise ctxt in
    let tezos_context =
      Memory_proto_alpha.Protocol.Alpha_context.Gas.set_limit
        tezos_context
        (Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.integral_exn
           (Z.of_int 800000))
    in
    let timestamp =
      Memory_proto_alpha.Protocol.Script_timestamp.of_zint
        (Z.of_int64
           (Proto_alpha_utils.Time.Protocol.to_seconds (Tezos_state.get_timestamp ctxt)))
    in
    let level =
      Memory_proto_alpha.Protocol.(
        (Alpha_context.Level.current tezos_context).level
        |> Alpha_context.Raw_level.to_int32
        |> Script_int.of_int32
        |> Script_int.abs)
    in
    Tezos_state.Tezos_protocol.
      { tezos_context
      ; source
      ; payer = source
      ; self = source
      ; amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez_exn 100000000L
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
  let open Lwt.Let_syntax in
  let%map runres =
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
  let expanded = Of_aggregated.compile_expression ~raise aggregated_exp in
  let mini_c_exp = Of_expanded.compile_expression ~raise expanded in
  Of_mini_c.compile_expression ~raise ~options mini_c_exp


let compile_type ~raise type_exp =
  let open Ligo_compile in
  let ty = Of_expanded.compile_type ~raise type_exp in
  Of_mini_c.compile_type ty


let entrypoint_of_string x =
  match
    Memory_proto_alpha.Raw_protocol.Entrypoint_repr.of_annot_lax_opt
      (Memory_proto_alpha.Raw_protocol.Non_empty_string.of_string_exn x)
  with
  | Some x -> x
  | None -> failwith (Format.asprintf "Testing framework: Invalid entrypoint %s" x)


let build_ast
    ~raise
    subst_lst
    mut_flag
    arg_binder
    rec_name
    in_ty
    out_ty
    (aggregated_exp : Ast_aggregated.expression)
  =
  let loc = aggregated_exp.location in
  let aggregated_exp' = add_ast_env subst_lst arg_binder aggregated_exp in
  let aggregated_exp =
    match rec_name with
    | None ->
      Ast_aggregated.e_a_lambda
        ~loc
        { result = aggregated_exp'
        ; output_type = out_ty
        ; binder = Ligo_prim.Param.make ~mut_flag arg_binder in_ty
        }
        in_ty
        out_ty
    | Some fun_name ->
      Ast_aggregated.e_a_recursive
        ~loc
        { fun_name
        ; fun_type = Ast_aggregated.t_arrow ~loc in_ty out_ty ()
        ; lambda =
            { result = aggregated_exp'
            ; output_type = out_ty
            ; binder = Ligo_prim.Param.make ~mut_flag arg_binder in_ty
            }
        ; force_lambdarec = false
        }
  in
  let parameter, storage =
    trace_option
      ~raise
      (Errors.generic_error Location.generated "Trying to compile a non-contract?")
    @@ Ast_aggregated.get_t_pair in_ty
  in
  trace ~raise Main_errors.self_ast_aggregated_tracer
  @@ Self_ast_aggregated.all_contract parameter storage aggregated_exp


let compile_contract_ast_none ~raise ~options ~tezos_context main =
  let open Lwt.Let_syntax in
  let open Ligo_compile in
  let expanded = Of_aggregated.compile_expression ~raise main in
  let mini_c = Of_expanded.compile_expression ~raise expanded in
  let%bind main_michelson = Of_mini_c.compile_contract ~raise ~options mini_c in
  let%map contract =
    Ligo_compile.Of_michelson.build_contract
      ~raise
      ~has_env_comments:false
      ~protocol_version:options.middle_end.protocol_version
      ~disable_typecheck:false
      ~tezos_context
      main_michelson
      []
  in
  Tezos_utils.Micheline.Micheline.map_node (fun _ -> ()) (fun x -> x) contract


let compile_contract_ast_single ~raise ~options ~tezos_context main views =
  let open Lwt.Let_syntax in
  let open Ligo_compile in
  let expanded = Of_aggregated.compile_expression ~raise main in
  let mini_c = Of_expanded.compile_expression ~raise expanded in
  let%bind main_michelson = Of_mini_c.compile_contract ~raise ~options mini_c in
  let%bind views =
    match views with
    | view_names, aggregated ->
      let mini_c =
        let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
        Of_expanded.compile_expression ~raise expanded
      in
      let mini_c =
        trace ~raise Main_errors.self_mini_c_tracer
        @@ Self_mini_c.all_expression options mini_c
      in
      let mini_c_tys =
        trace_option
          ~raise
          (`Self_mini_c_tracer
            (Self_mini_c.Errors.corner_case "Error reconstructing type of views"))
        @@ Mini_c.get_t_tuple mini_c.type_expression
      in
      let nb_of_views = List.length view_names in
      let aux i view =
        let idx_ty =
          trace_option
            ~raise
            (`Self_mini_c_tracer
              (Self_mini_c.Errors.corner_case "Error reconstructing type of view"))
          @@ List.nth mini_c_tys i
        in
        let idx = Mini_c.e_proj mini_c idx_ty i nb_of_views in
        view, idx
      in
      let views = List.mapi ~f:aux view_names in
      let aux (vn, mini_c) =
        let%map view = Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c in
        vn, view
      in
      let%map michelsons = Lwt_list.map_s aux views in
      let () =
        Ligo_compile.Of_michelson.check_view_restrictions
          ~raise
          (List.map ~f:snd michelsons)
      in
      michelsons
  in
  let disable_typecheck =
    match Sys.backend_type with
    | Sys.Other "js_of_ocaml" -> true
    | _ -> false
  in
  let%map contract =
    Ligo_compile.Of_michelson.build_contract
      ~raise
      ~has_env_comments:false
      ~protocol_version:options.middle_end.protocol_version
      ~disable_typecheck
      ~tezos_context
      main_michelson
      views
  in
  Tezos_utils.Micheline.Micheline.map_node (fun _ -> ()) (fun x -> x) contract


let compile_contract_ast_multi ~raise ~options ~tezos_context main views =
  let open Lwt.Let_syntax in
  let open Ligo_compile in
  let expanded = Of_aggregated.compile_expression ~raise main in
  let mini_c = Of_expanded.compile_expression ~raise expanded in
  let%bind main_michelson = Of_mini_c.compile_contract ~raise ~options mini_c in
  let f (view_name, aggregated) =
    let mini_c =
      let expanded = Ligo_compile.Of_aggregated.compile_expression ~raise aggregated in
      Of_expanded.compile_expression ~raise expanded
    in
    let mini_c =
      trace ~raise Main_errors.self_mini_c_tracer
      @@ Self_mini_c.all_expression options mini_c
    in
    let%map michelson = Ligo_compile.Of_mini_c.compile_view ~raise ~options mini_c in
    let () = Ligo_compile.Of_michelson.check_view_restrictions ~raise [ michelson ] in
    view_name, michelson
  in
  let%bind views = Lwt_list.map_s f views in
  let%map contract =
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


let compile_contract_file ~raise ~options source_file =
  let open Lwt.Let_syntax in
  let%map aggregated, views =
    Build.build_contract_meta_ligo ~raise ~options source_file
  in
  aggregated, views


let make_function ~loc mut_flag in_ty out_ty arg_binder body subst_lst =
  let typed_exp' = add_ast_env subst_lst arg_binder body in
  Ast_aggregated.e_a_lambda
    ~loc
    { result = typed_exp'
    ; output_type = out_ty
    ; binder = Ligo_prim.Param.make ~mut_flag arg_binder in_ty
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
    e_a_unit ~loc ()
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
    e_a_bool ~loc b
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
        | T_constant { injection = Int | External Int; _ } -> Some ()
        | _ -> None)
    in
    e_a_int ~loc x
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
    e_a_nat ~loc x
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
    e_a_mutez ~loc x
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
    e_a_timestamp ~loc t
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
    e_a_string ~loc (Simple_utils.Ligo_string.standard s)
  | V_Ct (C_bytes b) ->
    (match get_t_bytes ty with
    | Some () -> e_a_bytes ~loc b
    | None ->
      raise.error
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bytes but got %a"
              Ast_aggregated.PP.type_expression
              ty)))
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
    e_a_address ~loc x
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
    e_a_key_hash ~loc x
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
    e_a_key ~loc x
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
    e_a_signature ~loc x
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
    e_a_bls12_381_g1 ~loc x
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
    e_a_bls12_381_g2 ~loc x
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
    e_a_bls12_381_fr ~loc x
  | V_Ct (C_chest b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected chest but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_chest ty)
    in
    e_a_chest ~loc b
  | V_Ct (C_chest_key b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected chest_key but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_chest_key ty)
    in
    e_a_chest_key ~loc b
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
    let x = string_of_chain_id s in
    e_a_chain_id ~loc x
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
    let ty' = Ligo_prim.(Record.find map_ty.fields (Label.create ctor)) in
    let arg = val_to_ast ~raise ~loc arg ty' in
    e_a_constructor ~loc ctor arg ty
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
      v.arg_mut_flag
      v.arg_binder
      v.body
      v.orig_lambda
  | V_Michelson (Ty_code { micheline_repr = { code; code_ty = _ }; ast_ty }) ->
    let s = Format.asprintf "%a" Tezos_utils.Michelson.pp code in
    let s = Ligo_string.verbatim s in
    e_a_raw_code ~loc Backend.Michelson.name (make_e ~loc (e_string s) ast_ty) ast_ty
  | V_Record record when is_t_record ty ->
    let row =
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
    make_ast_record ~raise ~loc row record
  | V_Record map when Option.is_some @@ get_t_ticket ty ->
    let ty =
      trace_option ~raise (Errors.generic_error loc "impossible") @@ get_t_ticket ty
    in
    let row =
      trace_option ~raise (Errors.generic_error loc "impossible")
      @@ get_t_record (Ast_aggregated.t_unforged_ticket ~loc ty)
    in
    let map =
      let get l map =
        trace_option
          ~raise
          (Errors.generic_error loc "bad unforged ticket")
          (Ligo_prim.Record.find_opt map l)
      in
      (*  at this point the record value is a nested pair (extracted from michelson), e.g. (KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL , (0x05010000000474657374 , 10n)) *)
      let ticketer = get (Ligo_prim.Label.create "0") map in
      let map =
        match get (Ligo_prim.Label.create "1") map with
        | V_Record map -> map
        | _ -> raise.error @@ Errors.generic_error loc "unforged ticket badly decompiled"
      in
      let value = get (Ligo_prim.Label.create "0") map in
      let amt = get (Ligo_prim.Label.create "1") map in
      Ligo_prim.Record.of_list
        [ Ligo_prim.Label.create "ticketer", ticketer
        ; Ligo_prim.Label.create "value", value
        ; Ligo_prim.Label.create "amount", amt
        ]
    in
    make_ast_record ~raise ~loc row map
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
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: michelson-contract"
  | V_Ast_contract _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: ast-contract"
  | V_Michelson (Untyped_code _) ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: untyped-michelson-code"
  | V_Mutation _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: mutation"
  | V_Gen _ -> raise.error @@ Errors.generic_error loc "Cannot be abstracted: generator"
  | V_Location _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: location"
  | V_Typed_address _ ->
    raise.error @@ Errors.generic_error loc "Cannot be abstracted: typed_address"
  (* This is a hack to work around a bug with mutation on contract value
     i.e. `Test.mutation` called with `contract_of X`, see src/test/contracts/interpreter_tests/test_no_mutation.mligo*)
  | V_Views _ -> e_a_unit ~loc ()


and make_ast_func ~raise ?name env mut_flag arg body orig =
  let open Ast_aggregated in
  let loc = Location.interpreter in
  let env = make_subst_ast_env_exp ~raise env in
  let typed_exp' = add_ast_env ?name env arg body in
  let Ligo_prim.Arrow.{ type1 = in_ty; type2 = out_ty; param_names = _ } =
    get_t_arrow_exn orig.type_expression
  in
  let lambda =
    Ligo_prim.Lambda.
      { result = typed_exp'
      ; output_type = out_ty
      ; binder = Ligo_prim.Param.make ~mut_flag arg in_ty
      }
  in
  let typed_exp' =
    match name with
    | None -> e_a_lambda ~loc lambda in_ty out_ty
    | Some fun_name ->
      e_a_recursive
        ~loc
        { fun_name; fun_type = orig.type_expression; lambda; force_lambdarec = false }
  in
  (* Check that function to be compiled is obj-LIGO *)
  let _ =
    trace ~raise Main_errors.self_ast_aggregated_tracer
    @@ Self_ast_aggregated.expression_obj typed_exp'
  in
  typed_exp'


and make_ast_record ~raise ~loc (row : _ Ligo_prim.Row.With_layout.t) map =
  let open Ligo_interpreter.Types in
  let kv_list = Row.to_alist row in
  let kv_list =
    List.map
      ~f:(fun (l, ty) ->
        let value = Ligo_prim.Record.find map l in
        let ast = val_to_ast ~raise ~loc value ty in
        l, ast)
      kv_list
  in
  (* hmm *)
  Ast_aggregated.ez_e_a_record_hmm ~loc ~layout:row.layout kv_list


and make_ast_list ~raise ~loc ty l =
  let l = List.map ~f:(fun v -> val_to_ast ~raise ~loc v ty) l in
  List.fold_right
    l
    ~f:(Ast_aggregated.e_a_cons ~loc)
    ~init:(Ast_aggregated.e_a_nil ~loc ty)


and make_ast_set ~raise ~loc ty l =
  let l = List.dedup_and_sort ~compare:Ligo_interpreter.Combinators.compare_value l in
  let l = List.map ~f:(fun v -> val_to_ast ~raise ~loc v ty) l in
  List.fold_right
    l
    ~f:(Ast_aggregated.e_a_set_add ~loc)
    ~init:(Ast_aggregated.e_a_set_empty ~loc ty)


and make_ast_big_map ~raise ~loc key_ty value_ty kv =
  let kv =
    List.dedup_and_sort
      ~compare:(fun (k, _) (k', _) -> Ligo_interpreter.Combinators.compare_value k k')
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
    ~f:(fun (k, v) r -> Ast_aggregated.e_a_big_map_add ~loc k v r)
    ~init:(Ast_aggregated.e_a_big_map_empty ~loc key_ty value_ty)


and make_ast_map ~raise ~loc key_ty value_ty kv =
  let kv =
    List.dedup_and_sort
      ~compare:(fun (k, _) (k', _) -> Ligo_interpreter.Combinators.compare_value k k')
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
    ~f:(fun (k, v) r -> Ast_aggregated.e_a_map_add ~loc k v r)
    ~init:(Ast_aggregated.e_a_map_empty ~loc key_ty value_ty)


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
    -> Ligo_interpreter.Types.mcode Lwt.t
  =
 fun v ty ->
  let open Lwt.Let_syntax in
  let open Ast_aggregated in
  let open Ast_aggregated.Combinators in
  let self = compile_value ~raise ~options ~loc in
  let no_colour =
    let open Compiler_options in
    options.backend.no_colour
  in
  let pp_value = Ligo_interpreter.PP.pp_value ~no_colour in
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
    Lwt.return @@ Tezos_micheline.Micheline.String ((), s)
  | V_Ct (C_bytes b) ->
    Lwt.return
    @@
    (match get_t_bytes ty with
    | Some () -> Tezos_micheline.Micheline.Bytes ((), b)
    | None ->
      raise.error
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected bytes but got %a"
              Ast_aggregated.PP.type_expression
              ty)))
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
    Lwt.return @@ Tezos_micheline.Micheline.Int ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.Int ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.Int ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.Prim ((), "Unit", [], [])
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
    Lwt.return @@ Tezos_micheline.Micheline.Prim ((), "True", [], [])
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
    Lwt.return @@ Tezos_micheline.Micheline.Prim ((), "False", [], [])
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
    Lwt.return @@ Tezos_micheline.Micheline.String ((), x)
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
    Lwt.return
    @@
    (match c.entrypoint with
    | None -> Tezos_micheline.Micheline.String ((), x)
    | Some e ->
      Tezos_micheline.Micheline.String
        ((), Format.asprintf "%s%%%a" x Ligo_interpreter.Types.Entrypoint_repr.pp e))
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
    Lwt.return @@ Tezos_micheline.Micheline.String ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.String ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.String ((), x)
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
    let x = string_of_chain_id s in
    Lwt.return @@ Tezos_micheline.Micheline.String ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.Bytes ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.Bytes ((), x)
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
    Lwt.return @@ Tezos_micheline.Micheline.Bytes ((), x)
  | V_Ct (C_chest b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected chest but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_chest ty)
    in
    Lwt.return @@ Tezos_micheline.Micheline.Bytes ((), b)
  | V_Ct (C_chest_key b) ->
    let () =
      trace_option
        ~raise
        (Errors.generic_error
           loc
           (Format.asprintf
              "Expected chest_key but got %a"
              Ast_aggregated.PP.type_expression
              ty))
        (get_t_chest_key ty)
    in
    Lwt.return @@ Tezos_micheline.Micheline.Bytes ((), b)
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
    Lwt.return @@ Tezos_micheline.Micheline.Int ((), t)
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
    Lwt.return @@ Tezos_micheline.Micheline.Int ((), Z.of_int64 x)
  | V_Construct (ctor, arg) when Option.is_some (get_t_option ty) ->
    (match ctor with
    | "None" -> Lwt.return @@ Tezos_micheline.Micheline.Prim ((), "None", [], [])
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
      let%map arg = self arg option_ty in
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
    let ty' = Ligo_prim.(Record.find map_ty.fields (Label.create ctor)) in
    let%map arg = self arg ty' in
    let ty' = Ligo_compile.Of_expanded.compile_type ~raise ty in
    let ty_variant =
      trace_option ~raise (Errors.generic_error Location.generated "foo")
      @@ get_t_sum_opt ty
    in
    let path =
      Spilling.Layout.constructor_to_lr
        ~layout:ty_variant.layout
        ty'
        (Ligo_prim.Label.create ctor)
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
    let%map map_kv =
      Lwt.map Ligo_prim.Record.of_list
      @@ Lwt_list.map_s (fun (l, v) ->
             let ty = Ligo_prim.Record.find map_ty.fields l in
             let%map v = self v ty in
             l, v)
      @@ Ligo_prim.Record.to_list map
    in
    trace ~raise Main_errors.spilling_tracer
    @@ Spilling.Layout.from_layout
         (fun types ->
           let types = List.map ~f:snd types in
           match types with
           | [] -> Tezos_micheline.Micheline.Prim ((), "Unit", [], [])
           | [ type_ ] -> type_
           | types -> Tezos_micheline.Micheline.Prim ((), "Pair", types, []))
         map_kv
         map_ty.layout
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
    let%map lst = Lwt_list.map_s (fun v -> self v lst_ty) lst in
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
    let%map lst = Lwt_list.map_s (fun v -> self v lst_ty) lst in
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
    let%map map =
      Lwt_list.map_s
        (fun (k, v) ->
          let%bind k = self k k_ty in
          let%map v = self v v_ty in
          k, v)
        map
    in
    let map =
      List.map
        ~f:(fun (k, v) -> Tezos_micheline.Micheline.Prim ((), "Elt", [ k; v ], []))
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
    let%map map =
      Lwt_list.map_s
        (fun (k, v) ->
          let%bind k = self k k_ty in
          let%map v = self v v_ty in
          k, v)
        map
    in
    let map =
      List.map
        ~f:(fun (k, v) -> Tezos_micheline.Micheline.Prim ((), "Elt", [ k; v ], []))
        map
    in
    Tezos_micheline.Micheline.Seq ((), map)
  | V_Func_val v ->
    let make_subst_ast_env_exp ~raise env =
      let open Ligo_interpreter.Types in
      let rec aux acc = function
        | [] -> Lwt.return acc
        | (name, { item; no_mutation; inline }) :: tl ->
          let%bind mich = self item.eval_term item.ast_type in
          let minic_ty = Ligo_compile.Of_expanded.compile_type ~raise item.ast_type in
          let mich_ty = Ligo_compile.Of_mini_c.compile_type minic_ty in
          let mich_ty =
            Tezos_micheline.(Micheline.map_node (fun _ -> ()) (fun x -> x) mich_ty)
          in
          let mich =
            Tezos_micheline.Micheline.(
              Seq
                ( ()
                , [ Prim ((), "DROP", [], []); Prim ((), "PUSH", [ mich_ty; mich ], []) ]
                ))
          in
          let mich =
            Tezos_micheline.(
              Micheline.map_node
                (fun _ -> Micheline_printer.{ comment = None })
                (fun x -> x)
                mich)
          in
          let mich =
            Format.asprintf "%a" Tezos_micheline.Micheline_printer.print_expr mich
          in
          let expr =
            e_a_raw_code
              ~loc
              "Michelson"
              (make_e
                 ~loc
                 (e_string @@ Ligo_string.verbatim mich)
                 (t_arrow ~loc (t_unit ~loc ()) item.ast_type ()))
              (t_arrow ~loc (t_unit ~loc ()) item.ast_type ())
          in
          let expr = e_a_application ~loc expr (e_a_unit ~loc ()) item.ast_type in
          aux ((name, expr, no_mutation, inline) :: acc) tl
      in
      aux [] env
    in
    let make_ast_func ~raise ?name env arg mut_flag body orig =
      let open Ast_aggregated in
      let%map env = make_subst_ast_env_exp ~raise env in
      let typed_exp' = add_ast_env ?name env arg body in
      let Ligo_prim.Arrow.{ type1 = in_ty; type2 = out_ty; param_names = _ } =
        get_t_arrow_exn orig.type_expression
      in
      let lambda =
        Ligo_prim.Lambda.
          { result = typed_exp'
          ; output_type = out_ty
          ; binder = Ligo_prim.Param.make ~mut_flag arg in_ty
          }
      in
      let typed_exp' =
        match name with
        | None -> e_a_lambda ~loc lambda in_ty out_ty
        | Some fun_name ->
          e_a_recursive
            ~loc
            { fun_name; fun_type = orig.type_expression; lambda; force_lambdarec = false }
      in
      (* Check that function to be compiled is obj-LIGO *)
      let _ =
        trace ~raise Main_errors.self_ast_aggregated_tracer
        @@ Self_ast_aggregated.expression_obj typed_exp'
      in
      typed_exp'
    in
    let%bind typed_exp =
      make_ast_func
        ~raise
        ?name:v.rec_name
        v.env
        v.arg_binder
        v.arg_mut_flag
        v.body
        v.orig_lambda
    in
    let%map compiled_exp = compile_ast ~raise ~options typed_exp in
    (match compiled_exp.expr with
    | Seq (_, [ Prim (_, "LAMBDA", [ _; _; compiled_exp ], _) ]) ->
      let compiled_exp =
        Tezos_micheline.Micheline.map_node (fun _ -> ()) (fun x -> x) compiled_exp
      in
      compiled_exp
    | _ -> raise.error @@ Errors.generic_error loc (Format.asprintf "Expected LAMBDA"))
  | V_Michelson (Ty_code x) -> Lwt.return @@ x.micheline_repr.code
  | V_Michelson (Untyped_code x) -> Lwt.return @@ x
  | v ->
    raise.error
    @@ Errors.generic_error
         loc
         (Format.asprintf
            "Cannot decompile value %a of type %a"
            pp_value
            v
            Ast_aggregated.PP.type_expression
            ty)


let compile_type_to_mcode ~raise
    : Ast_aggregated.type_expression -> Ligo_interpreter.Types.mcode
  =
 fun ty ->
  let expr_ty = Ligo_compile.Of_expanded.compile_type ~raise ty in
  let expr_ty = Ligo_compile.Of_mini_c.compile_type expr_ty in
  let expr_ty = clean_location_with () expr_ty in
  expr_ty


let compile_value ~raise ~options ~loc
    :  Ligo_interpreter.Types.value -> Ast_aggregated.type_expression
    -> Ligo_interpreter.Types.typed_michelson_code Lwt.t
  =
 fun v ty ->
  let open Lwt.Let_syntax in
  let%map expr = compile_value ~raise ~options ~loc v ty in
  let expr_ty = Ligo_compile.Of_expanded.compile_type ~raise ty in
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
    result_ty
    arg
    arg_ty
  =
  let open Lwt.Let_syntax in
  let open Ligo_interpreter.Types in
  let%bind run_options = make_options ~raise (Some ctxt) in
  let%bind { micheline_repr = { code = arg; code_ty = arg_ty }; _ } =
    compile_value ~raise ~options ~loc arg arg_ty
  in
  let result_ty_ = compile_type ~raise result_ty in
  let func =
    match code with
    | Seq (_, s) -> Tezos_utils.Michelson.(seq ([ i_push arg_ty arg ] @ s))
    | _ -> raise.error (Errors.generic_error Location.generated "Could not parse")
  in
  match%map
    Ligo_run.Of_michelson.run_expression
      ~raise
      ~legacy:true
      ~options:run_options
      func
      result_ty_
  with
  | Success (ty, value) ->
    let v =
      Michelson_to_value.decompile_to_untyped_value
        ~raise
        ~bigmaps:ctxt.transduced.bigmaps
        ty
        value
    in
    Result.return
    @@ Michelson_to_value.decompile_value
         ~raise
         ~bigmaps:ctxt.transduced.bigmaps
         v
         result_ty
  | Fail f -> Result.fail f


let run_michelson_func_
    ~raise
    ~options
    ~loc
    (ctxt : Tezos_state.context)
    (code : (unit, string) Tezos_micheline.Micheline.node)
    result_ty
    args
  =
  let open Lwt.Let_syntax in
  let open Ligo_interpreter.Types in
  let%bind run_options = make_options ~raise (Some ctxt) in
  let%bind args =
    Lwt_list.map_s
      (fun (arg, arg_ty) ->
        let%map { micheline_repr = { code = arg; code_ty = arg_ty }; _ } =
          compile_value ~raise ~options ~loc arg arg_ty
        in
        arg, arg_ty)
      args
  in
  let result_ty_ = compile_type ~raise result_ty in
  let args =
    List.fold_right
      ~f:(fun (arg, arg_ty) pushes -> Tezos_utils.Michelson.i_push arg_ty arg :: pushes)
      ~init:[]
      args
  in
  let args = List.rev args in
  let func =
    match code with
    | Seq (_, s) -> Tezos_utils.Michelson.(seq (args @ s))
    | _ -> raise.error (Errors.generic_error Location.generated "Could not parse")
  in
  match%map
    Ligo_run.Of_michelson.run_expression
      ~raise
      ~legacy:true
      ~options:run_options
      func
      result_ty_
  with
  | Success (ty, value) ->
    let v =
      Michelson_to_value.decompile_to_untyped_value
        ~raise
        ~bigmaps:ctxt.transduced.bigmaps
        ty
        value
    in
    Result.return
    @@ Michelson_to_value.decompile_value
         ~raise
         ~bigmaps:ctxt.transduced.bigmaps
         v
         result_ty
  | Fail f -> Result.fail f


let parse_code ~raise code =
  let open Tezos_micheline in
  let code, errs = Micheline_parser.tokenize code in
  let code =
    match errs with
    | _ :: _ -> raise.error (Errors.generic_error Location.generated "Could not parse")
    | [] ->
      let code, errs = Micheline_parser.parse_expression ~check:false code in
      (match errs with
      | _ :: _ -> raise.error (Errors.generic_error Location.generated "Could not parse")
      | [] ->
        let code = Micheline.map_node (fun _ -> ()) (fun x -> x) code in
        (match code with
        | Seq (_, s) -> Tezos_utils.Michelson.(seq s)
        | _ -> raise.error (Errors.generic_error Location.generated "Could not parse")))
  in
  code


let parse_raw_michelson_code ~raise code ty =
  let open Tezos_micheline in
  let ty = compile_type ~raise ty in
  let code = parse_code ~raise code in
  let code_ty = Micheline.map_node (fun _ -> ()) (fun x -> x) ty in
  code, code_ty


let compare_michelson ~raise loc a b =
  let module LT = Ligo_interpreter.Types in
  let module LC = Ligo_interpreter.Combinators in
  let (code, _) : LT.mcode * _ =
    trace_option ~raise (Errors.generic_error loc "Can't compare contracts")
    @@ LC.get_michelson_code_and_type a
  in
  let (code', _) : LT.mcode * _ =
    trace_option ~raise (Errors.generic_error loc "Can't compare contracts")
    @@ LC.get_michelson_code_and_type b
  in
  Caml.compare code code'
