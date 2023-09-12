open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace
open Ligo_prim

let check_view_restrictions ~raise : Stacking.compiled_expression list -> unit =
 fun views_mich ->
  (* From Tezos changelog on views:
    CREATE_CONTRACT, SET_DELEGATE and TRANSFER_TOKENS cannot be used at the top-level of a
    view because they are stateful, and SELF because the entry-point does not make sense in a view.
    However, CREATE_CONTRACT, SET_DELEGATE and TRANSFER_TOKENS remain available in lambdas defined inside a view. (MR !3737)
  *)
  let open Tezos_micheline.Micheline in
  let rec iter_prim_mich
      :  inside_lambda:bool -> (inside_lambda:bool -> 'loc * 'p -> unit)
      -> ('loc, 'p) node -> unit
    =
   fun ~inside_lambda f m ->
    match m with
    | Int _ | String _ | Bytes _ -> ()
    | Seq (_, x) -> List.iter ~f:(iter_prim_mich ~inside_lambda f) x
    | Prim (loc, p, lst, _) ->
      f ~inside_lambda (loc, p);
      let inside_lambda = if String.equal p "LAMBDA" then true else false in
      List.iter ~f:(iter_prim_mich ~inside_lambda f) lst
  in
  let iter_prim_mich = iter_prim_mich ~inside_lambda:false in
  let f ~inside_lambda : Mini_c.meta * string -> unit =
   fun ({ location; _ }, prim_str) ->
    match prim_str, inside_lambda with
    | "SELF", (true | false) -> raise.error (`Main_view_rule_violated location)
    | ("CREATE_CONTRACT" | "SET_DELEGATE" | "TRANSFER_TOKENS"), false ->
      raise.error (`Main_view_rule_violated location)
    | _ -> ()
  in
  List.iter ~f:(fun m -> iter_prim_mich f m.expr) views_mich


let parse_constant ~raise code =
  let open Tezos_micheline in
  let open Tezos_micheline.Micheline in
  let code, errs = Micheline_parser.tokenize code in
  let code =
    match errs with
    | _ :: _ ->
      raise.error
        (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
    | [] ->
      let code, errs = Micheline_parser.parse_expression ~check:false code in
      (match errs with
      | _ :: _ ->
        raise.error
          (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
      | [] -> map_node (fun _ -> ()) (fun x -> x) code)
  in
  Trace.trace_alpha_tzresult ~raise unparsing_michelson_tracer
  @@ Memory_proto_alpha.node_to_canonical code


let parse_constant_pre ~raise code =
  let open Tezos_micheline in
  let open Tezos_micheline.Micheline in
  let code, errs = Micheline_parser.tokenize code in
  let code =
    match errs with
    | _ :: _ ->
      raise.error
        (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
    | [] ->
      let code, errs = Micheline_parser.parse_expression ~check:false code in
      (match errs with
      | _ :: _ ->
        raise.error
          (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
      | [] -> map_node (fun _ -> ()) (fun x -> x) code)
  in
  Proto_pre_alpha_utils.(
    Trace.trace_alpha_tzresult ~raise unparsing_michelson_tracer
    @@ Memory_proto_alpha.node_to_canonical code)


let dummy : Stacking.meta =
  { location = Location.dummy; env = []; binder = None; source_type = None }


(* should preserve locations, currently wipes them *)
let build_contract ~raise
    :  protocol_version:Environment.Protocols.t -> ?enable_typed_opt:bool
    -> ?has_env_comments:bool -> ?disable_typecheck:bool -> ?constants:string list
    -> ?tezos_context:_ -> Stacking.compiled_expression
    -> (Value_var.t * Stacking.compiled_expression) list -> _ Michelson.michelson Lwt.t
  =
 fun ~protocol_version
     ?(enable_typed_opt = false)
     ?(has_env_comments = false)
     ?(disable_typecheck = false)
     ?(constants = [])
     ?tezos_context
     compiled
     views ->
  let open Lwt.Let_syntax in
  let build_view_f (name, (view : Stacking.compiled_expression)) =
    let view_param_ty, ret_ty =
      trace_option ~raise (main_view_not_a_function name)
      @@ (* remitodo error specific to views*)
      Self_michelson.fetch_views_ty view.expr_ty
    in
    Value_var.to_name_exn name, view_param_ty, ret_ty, view.expr
  in
  let views = List.map ~f:build_view_f views in
  let param_ty, storage_ty =
    trace_option ~raise main_entrypoint_not_a_function
    @@ Self_michelson.fetch_contract_ty_inputs compiled.expr_ty
  in
  let expr = compiled.expr in
  let contract =
    Michelson.lcontract dummy dummy param_ty dummy storage_ty dummy expr dummy views
  in
  if disable_typecheck
  then Lwt.return contract
  else if Environment.Protocols.equal Environment.Protocols.in_use protocol_version
  then (
    let%bind contract' =
      Lwt.map
        (Trace.trace_tzresult
           ~raise
           (typecheck_contract_tracer protocol_version contract))
        (Memory_proto_alpha.prims_of_strings contract)
    in
    (* Parse constants *)
    let constants = List.map ~f:(parse_constant ~raise) constants in
    let%bind environment =
      match tezos_context with
      | None -> Proto_alpha_utils.Memory_proto_alpha.dummy_environment ()
      | Some tezos_context ->
        Lwt.return Proto_alpha_utils.Init_proto_alpha.{ tezos_context; identities = [] }
    in
    (* Update the Tezos context by registering the global constants *)
    let%bind tezos_context =
      Lwt_list.fold_left_s
        (fun ctxt cnt ->
          let%map ctxt, _, _ =
            Lwt.map
              (Trace.trace_alpha_tzresult
                 ~raise
                 (typecheck_contract_tracer protocol_version contract))
            @@ Proto_alpha_utils.Memory_proto_alpha.register_constant ctxt cnt
          in
          ctxt)
        environment.tezos_context
        constants
    in
    let environment = { environment with tezos_context } in
    (* Type-check *)
    let%bind (_ : (_, _) Micheline.Micheline.node) =
      Lwt.map
        (Trace.trace_tzresult
           ~raise
           (typecheck_contract_tracer protocol_version contract))
      @@ Proto_alpha_utils.Memory_proto_alpha.typecheck_contract ~environment contract'
    in
    if enable_typed_opt
    then (
      let typer_oracle : type a. (a, _) Micheline.Micheline.node -> _ Lwt.t =
       fun c ->
        let%map map, _ =
          Lwt.map
            (Trace.trace_tzresult
               ~raise
               (typecheck_contract_tracer protocol_version contract))
          @@ Proto_alpha_utils.Memory_proto_alpha.typecheck_map_contract ~environment c
        in
        map
      in
      let has_comment : Mini_c.meta -> bool =
       fun { env; location; binder = _; source_type = _ } ->
        has_env_comments
        && ((not (List.is_empty env)) || not (Location.is_dummy_or_generated location))
      in
      Self_michelson.optimize_with_types
        ~raise
        ~typer_oracle
        protocol_version
        ~has_comment
        contract)
    else Lwt.return contract)
  else (
    let%bind contract' =
      Lwt.map
        (Trace.trace_tzresult
           ~raise
           (typecheck_contract_tracer protocol_version contract))
        (Proto_pre_alpha_utils.Memory_proto_alpha.prims_of_strings contract)
    in
    (* Parse constants *)
    let constants = List.map ~f:(parse_constant_pre ~raise) constants in
    let%bind environment =
      Proto_pre_alpha_utils.Memory_proto_alpha.dummy_environment ()
    in
    (* Update the Tezos context by registering the global constants *)
    let%bind tezos_context =
      Lwt_list.fold_left_s
        (fun ctxt cnt ->
          let%map ctxt, _, _ =
            Lwt.map
              (Proto_pre_alpha_utils.Trace.trace_alpha_tzresult
                 ~raise
                 (typecheck_contract_tracer protocol_version contract))
            @@ Proto_pre_alpha_utils.Memory_proto_alpha.register_constant ctxt cnt
          in
          ctxt)
        environment.tezos_context
        constants
    in
    let environment = { environment with tezos_context } in
    (* Type-check *)
    let%map (_ : (_, _) Micheline.Micheline.node) =
      Lwt.map
        (Proto_pre_alpha_utils.Trace.trace_tzresult
           ~raise
           (typecheck_contract_tracer protocol_version contract))
      @@ Proto_pre_alpha_utils.Memory_proto_alpha.typecheck_contract
           ~environment
           contract'
    in
    contract)


let measure ~raise m =
  Lwt.map (Trace.trace_tzresult ~raise main_could_not_serialize)
  @@ Proto_alpha_utils.Measure.measure m
