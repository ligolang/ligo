open Main_errors
open Tezos_utils
open Proto_alpha_utils
open Trace

let parse_constant ~raise code =
  let open Tezos_micheline in
  let open Tezos_micheline.Micheline in
  let (code, errs) = Micheline_parser.tokenize code in
  let code = (match errs with
              | _ :: _ -> raise.raise (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
              | [] ->
                 let (code, errs) = Micheline_parser.parse_expression ~check:false code in
                 match errs with
                 | _ :: _ -> raise.raise (unparsing_michelson_tracer @@ List.map ~f:(fun x -> `Tezos_alpha_error x) errs)
                 | [] -> map_node (fun _ -> ()) (fun x -> x) code
             ) in
  Trace.trace_alpha_tzresult ~raise unparsing_michelson_tracer @@
    Memory_proto_alpha.node_to_canonical code

let dummy : Stacking.meta =
  { location = Location.dummy;
    env = [];
    binder = None }

(* should preserve locations, currently wipes them *)
let build_contract ~raise :
  ?disable_typecheck:bool ->
  ?constants:string list ->
  Stacking.compiled_expression ->
  (Ast_typed.expression_variable * Stacking.compiled_expression) list -> _ Michelson.michelson  =
    fun ?(disable_typecheck= false) ?(constants = []) compiled views ->
      let views =
        List.map
          ~f:(fun (name, view) ->
            let (view_param_ty, ret_ty) = trace_option ~raise (main_view_not_a_function name) @@ (* remitodo error specific to views*)
              Self_michelson.fetch_views_ty view.expr_ty
            in
            (Ast_typed.ValueVar.to_name_exn name, view_param_ty, ret_ty, view.expr)
          )
          views
      in
      let (param_ty, storage_ty) = trace_option ~raise (main_entrypoint_not_a_function) @@
        Self_michelson.fetch_contract_ty_inputs compiled.expr_ty in
      let expr = compiled.expr in
      let contract =
        Michelson.lcontract
          dummy
          dummy param_ty
          dummy storage_ty
          dummy expr
          dummy views in
      if disable_typecheck then
        contract
      else
        let contract' =
          Trace.trace_tzresult_lwt ~raise (typecheck_contract_tracer contract)
            (Memory_proto_alpha.prims_of_strings contract) in
        let environment = Proto_alpha_utils.Memory_proto_alpha.dummy_environment () in
        (* Parse constants *)
        let constants = List.map ~f:(parse_constant ~raise) constants in
        (* Update the Tezos context by registering the global constants *)
        let tezos_context = List.fold_left constants ~init:environment.tezos_context
                    ~f:(fun ctxt cnt ->
                      let (ctxt, _, _) = Trace.trace_alpha_tzresult_lwt ~raise (typecheck_contract_tracer contract) @@
                                           Proto_alpha_utils.Memory_proto_alpha.register_constant ctxt cnt in
                      ctxt) in
        let environment = { environment with tezos_context } in
        (* Type-check *)
        let _ = Trace.trace_tzresult_lwt ~raise (typecheck_contract_tracer contract) @@
          Proto_alpha_utils.Memory_proto_alpha.typecheck_contract ~environment contract' in
        contract

let measure ~raise = fun m ->
  Trace.trace_tzresult_lwt ~raise (main_could_not_serialize) @@
    Proto_alpha_utils.Measure.measure m
